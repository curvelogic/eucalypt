//! Debouncing of background pipeline runs.
//!
//! Every edit to a document asks for a type-check pipeline run, but only
//! the last edit of a burst should start one, once the document has been
//! quiet for [`DEBOUNCE_DELAY`]. A single persistent thread, the
//! [`Debouncer`], receives the requests over a channel, keeps at most one
//! pending run per document in [`PendingRuns`], and hands each run to a
//! runner once its deadline passes. Edits therefore start no thread of
//! their own; only a run that fires does (the server's runner starts the
//! pipeline on a worker thread, so a long check never holds up the
//! debounce of other documents).

use std::collections::HashMap;
use std::thread::JoinHandle;
use std::time::{Duration, Instant};

use crossbeam_channel::{RecvTimeoutError, Sender};
use lsp_types::Url;

/// How long a document must be quiet before its pipeline run starts.
pub const DEBOUNCE_DELAY: Duration = Duration::from_millis(300);

/// The pending runs, at most one per document, each with the instant it
/// becomes due.
///
/// A pure state machine: time is passed in, so its behaviour can be
/// tested deterministically.
#[derive(Debug)]
pub struct PendingRuns<J> {
    delay: Duration,
    pending: HashMap<Url, (Instant, J)>,
}

impl<J> PendingRuns<J> {
    /// An empty set of pending runs, each due `delay` after it is
    /// scheduled.
    pub fn new(delay: Duration) -> Self {
        Self {
            delay,
            pending: HashMap::new(),
        }
    }

    /// Schedule `job` for document `uri` at time `now`. It replaces any
    /// run already pending for `uri`, and is due `delay` after `now`.
    pub fn schedule(&mut self, uri: Url, job: J, now: Instant) {
        self.pending.insert(uri, (now + self.delay, job));
    }

    /// The earliest instant at which a pending run is due, if any is
    /// pending.
    pub fn next_deadline(&self) -> Option<Instant> {
        self.pending.values().map(|(due, _)| *due).min()
    }

    /// Remove and return every run due at or before `now`, earliest first.
    pub fn take_due(&mut self, now: Instant) -> Vec<J> {
        let due_uris: Vec<Url> = self
            .pending
            .iter()
            .filter(|(_, (due, _))| *due <= now)
            .map(|(uri, _)| uri.clone())
            .collect();
        let mut due: Vec<(Instant, J)> = due_uris
            .into_iter()
            .filter_map(|uri| self.pending.remove(&uri))
            .collect();
        due.sort_by_key(|(instant, _)| *instant);
        due.into_iter().map(|(_, job)| job).collect()
    }

    /// The number of runs pending.
    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.pending.len()
    }

    /// Whether no run is pending.
    #[cfg(test)]
    pub fn is_empty(&self) -> bool {
        self.pending.is_empty()
    }
}

/// A persistent thread that debounces pipeline runs.
///
/// [`Debouncer::schedule`] only sends on a channel. The thread holds the
/// [`PendingRuns`], sleeps until the earliest deadline or the next
/// request, and calls the runner with each run that falls due. The
/// thread ends once the `Debouncer` is dropped; runs still pending then
/// are dropped without running.
pub struct Debouncer<J: Send + 'static> {
    sender: Option<Sender<(Url, J)>>,
    thread: Option<JoinHandle<()>>,
}

impl<J: Send + 'static> Debouncer<J> {
    /// Start the debounce thread. `run` is called on that thread with
    /// each run that falls due, so it should return promptly (start a
    /// worker for anything long).
    pub fn new(delay: Duration, run: impl Fn(J) + Send + 'static) -> std::io::Result<Self> {
        let (sender, receiver) = crossbeam_channel::unbounded::<(Url, J)>();
        let thread = std::thread::Builder::new()
            .name("eu-lsp-debounce".to_string())
            .spawn(move || {
                let mut pending = PendingRuns::new(delay);
                loop {
                    let request = match pending.next_deadline() {
                        Some(deadline) => match receiver.recv_deadline(deadline) {
                            Ok(request) => Some(request),
                            Err(RecvTimeoutError::Timeout) => None,
                            Err(RecvTimeoutError::Disconnected) => return,
                        },
                        None => match receiver.recv() {
                            Ok(request) => Some(request),
                            Err(_) => return,
                        },
                    };
                    if let Some((uri, job)) = request {
                        pending.schedule(uri, job, Instant::now());
                    }
                    for job in pending.take_due(Instant::now()) {
                        run(job);
                    }
                }
            })?;
        Ok(Self {
            sender: Some(sender),
            thread: Some(thread),
        })
    }

    /// Ask for a run of `job` for document `uri`, replacing any run still
    /// pending for it and restarting its delay.
    pub fn schedule(&self, uri: Url, job: J) {
        if let Some(sender) = &self.sender {
            // The thread only stops once the sender is dropped, so a send
            // fails only if it has panicked; the run is then lost, as a
            // run on a failed thread would be.
            let _ = sender.send((uri, job));
        }
    }
}

impl<J: Send + 'static> Drop for Debouncer<J> {
    fn drop(&mut self) {
        // Dropping the sender disconnects the channel, which ends the
        // thread at its next wake; it never blocks on a run itself.
        self.sender.take();
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn uri(name: &str) -> Url {
        Url::parse(&format!("file:///{name}.eu")).unwrap()
    }

    const DELAY: Duration = Duration::from_millis(300);

    #[test]
    fn a_burst_of_edits_yields_one_run_with_the_last_text() {
        let start = Instant::now();
        let mut pending = PendingRuns::new(DELAY);
        // Twenty keystrokes, 50 ms apart: nothing falls due during the
        // burst, and only one run is ever pending.
        for i in 0..20u64 {
            let now = start + Duration::from_millis(50 * i);
            pending.schedule(uri("a"), format!("text {i}"), now);
            assert!(pending.take_due(now).is_empty(), "nothing due mid-burst");
            assert_eq!(pending.len(), 1);
        }
        let last = start + Duration::from_millis(50 * 19);
        assert_eq!(pending.next_deadline(), Some(last + DELAY));
        assert!(pending
            .take_due(last + DELAY - Duration::from_millis(1))
            .is_empty());
        assert_eq!(pending.take_due(last + DELAY), vec!["text 19".to_string()]);
        assert!(pending.is_empty());
        assert_eq!(pending.next_deadline(), None);
    }

    #[test]
    fn a_new_edit_restarts_the_delay() {
        let start = Instant::now();
        let mut pending = PendingRuns::new(DELAY);
        pending.schedule(uri("a"), "first", start);
        let later = start + Duration::from_millis(250);
        pending.schedule(uri("a"), "second", later);
        // The first deadline has passed, but the second edit replaced it.
        assert!(pending.take_due(start + DELAY).is_empty());
        assert_eq!(pending.take_due(later + DELAY), vec!["second"]);
    }

    #[test]
    fn documents_are_debounced_independently() {
        let start = Instant::now();
        let mut pending = PendingRuns::new(DELAY);
        pending.schedule(uri("a"), "a", start);
        pending.schedule(uri("b"), "b", start + Duration::from_millis(100));
        assert_eq!(pending.len(), 2);
        assert_eq!(pending.next_deadline(), Some(start + DELAY));
        assert_eq!(pending.take_due(start + DELAY), vec!["a"]);
        assert_eq!(
            pending.take_due(start + Duration::from_millis(400)),
            vec!["b"]
        );
    }

    #[test]
    fn due_runs_come_earliest_first() {
        let start = Instant::now();
        let mut pending = PendingRuns::new(DELAY);
        pending.schedule(uri("late"), "late", start + Duration::from_millis(20));
        pending.schedule(uri("early"), "early", start);
        assert_eq!(pending.take_due(start + DELAY * 2), vec!["early", "late"]);
    }

    #[test]
    fn the_debounce_thread_runs_only_the_last_of_a_burst() {
        let (ran_tx, ran_rx) = crossbeam_channel::unbounded();
        // Long enough that the twenty sends below (microseconds apart)
        // all land inside one delay.
        let debouncer = Debouncer::new(Duration::from_millis(500), move |job: String| {
            let _ = ran_tx.send((job, Instant::now()));
        })
        .unwrap();
        let mut last_sent = Instant::now();
        for i in 0..20 {
            last_sent = Instant::now();
            debouncer.schedule(uri("a"), format!("text {i}"));
        }
        let (job, ran_at) = ran_rx
            .recv_timeout(Duration::from_secs(10))
            .expect("the run fires");
        assert_eq!(job, "text 19");
        assert!(ran_at >= last_sent + Duration::from_millis(500));
        // Dropping the debouncer joins its thread, so nothing more can run.
        drop(debouncer);
        assert!(ran_rx.try_recv().is_err(), "only one run");
    }

    #[test]
    fn dropping_the_debouncer_drops_pending_runs() {
        let (ran_tx, ran_rx) = crossbeam_channel::unbounded();
        let debouncer = Debouncer::new(Duration::from_secs(60), move |job: String| {
            let _ = ran_tx.send(job);
        })
        .unwrap();
        debouncer.schedule(uri("a"), "pending".to_string());
        drop(debouncer);
        assert!(ran_rx.try_recv().is_err());
    }
}
