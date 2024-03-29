//! Intrinsics for working with time and date

use std::convert::TryInto;

use chrono::{
    DateTime, Datelike, FixedOffset, LocalResult, NaiveDate, NaiveDateTime, NaiveTime, Offset,
    TimeZone, Timelike, Utc,
};
use chrono_tz::Tz;
use indexmap::IndexMap;
use regex::Regex;
use serde_json::Number;

use crate::{
    common::sourcemap::Smid,
    eval::{
        emit::Emitter,
        error::ExecutionError,
        machine::{
            env::Closure,
            intrinsic::{CallGlobal1, CallGlobal7, IntrinsicMachine, StgIntrinsic},
        },
        memory::{
            array::Array,
            mutator::MutatorHeapView,
            syntax::{Ref, StgBuilder},
        },
    },
};

use super::{
    support::{
        machine_return_block_pair_closure_list, machine_return_str, machine_return_zdt, num_arg,
        str_arg, zdt_arg,
    },
    tags::DataConstructor,
};

/// Convert a TZ representation to a FixedOffset
fn offset_from_tz_str(tz_str: &str) -> Result<FixedOffset, ExecutionError> {
    // TODO: find a way to use chrono to do this
    let regex = Regex::new(r#"([+-])(\d{2}):?(\d{2}):?(\d{2})?"#).unwrap();
    if let Some(captures) = regex.captures(tz_str) {
        let hour = captures
            .get(2)
            .map(|m| str::parse::<i32>(m.as_str()).unwrap())
            .unwrap_or(0);
        let min = captures
            .get(3)
            .map(|m| str::parse::<i32>(m.as_str()).unwrap())
            .unwrap_or(0);
        let sec = captures
            .get(4)
            .map(|m| str::parse::<i32>(m.as_str()).unwrap())
            .unwrap_or(0);

        let secs = hour * 3600 + min * 60 + sec;

        if let Some(sign) = captures.get(1) {
            if sign.as_str() == "-" {
                Ok(FixedOffset::east(secs))
            } else {
                Ok(FixedOffset::west(secs))
            }
        } else {
            Ok(FixedOffset::east(0))
        }
    } else if let Ok(zone) = tz_str.parse::<Tz>() {
        Ok(zone.ymd(2000, 1, 1).offset().fix())
    } else if tz_str == "Z" {
        Ok(FixedOffset::east(0))
    } else {
        Err(ExecutionError::BadTimeZone(tz_str.to_string()))
    }
}

/// ZDT(y, m, d, h, M, s, Z) - create a zoned date time
pub struct Zdt;

impl StgIntrinsic for Zdt {
    fn name(&self) -> &str {
        "ZDT"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let y = num_arg(machine, view, &args[0])?;
        let m = num_arg(machine, view, &args[1])?;
        let d = num_arg(machine, view, &args[2])?;
        let hour = num_arg(machine, view, &args[3])?;
        let min = num_arg(machine, view, &args[4])?;
        let sec = num_arg(machine, view, &args[5])?;
        let tz = str_arg(machine, view, &args[6])?;

        let err = || {
            ExecutionError::BadDateTimeComponents(
                y.clone(),
                m.clone(),
                d.clone(),
                hour.clone(),
                min.clone(),
                sec.clone(),
                tz.clone(),
            )
        };

        if let (Some(yy), Some(mm), Some(dd), Some(hours), Some(mins), Some(secs)) = (
            y.as_i64(),
            m.as_u64(),
            d.as_u64(),
            hour.as_u64(),
            min.as_u64(),
            sec.as_u64(),
        ) {
            // HACK: magic january
            // at the moment we tolerate use of zdt to represent naive
            // times in which case current prelude default y, m, d to
            // 0
            //
            // we need to make the year, month and day valid..
            let yy = if yy == 0 { 1 } else { yy };
            let mm = if mm == 0 { 1 } else { mm };
            let dd = if dd == 0 { 1 } else { dd };

            let date = NaiveDate::from_ymd_opt(
                yy.try_into().unwrap(),
                mm.try_into().unwrap(),
                dd.try_into().unwrap(),
            )
            .ok_or_else(err)?;

            let time = NaiveTime::from_hms_opt(
                hours.try_into().unwrap(),
                mins.try_into().unwrap(),
                secs.try_into().unwrap(),
            )
            .ok_or_else(err)?;

            let datetime = NaiveDateTime::new(date, time);
            let offset = offset_from_tz_str(&tz)?;
            if let LocalResult::Single(zdt) = offset.from_local_datetime(&datetime) {
                machine_return_zdt(machine, view, zdt)
            } else {
                Err(err())
            }
        } else {
            Err(err())
        }
    }
}

impl CallGlobal7 for Zdt {}

/// ZDT.FIELDS - convert ZDT to (sym, val) pairs (which can be
/// converted to block)
///
/// Timezone is always rendered as colon-less string e.g. "+0100"
pub struct ZdtFields;

impl StgIntrinsic for ZdtFields {
    fn name(&self) -> &str {
        "ZDT.FIELDS"
    }

    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let dt = zdt_arg(machine, view, &args[0])?;
        let y = dt.year();
        let m = dt.month();
        let d = dt.day();
        let hour = dt.hour();
        let min = dt.minute();
        let sec = (dt.second() as f64) + (dt.timestamp_subsec_millis() as f64) / 1000f64;
        let tz = dt.offset().to_string().replace(':', "");

        let mut fields = IndexMap::new();
        fields.insert(
            "y".to_string(),
            Closure::new(
                view.data(
                    DataConstructor::BoxedNumber.tag(),
                    Array::from_slice(&view, &[Ref::num(y)]),
                )?
                .as_ptr(),
                machine.root_env(),
            ),
        );
        fields.insert(
            "m".to_string(),
            Closure::new(
                view.data(
                    DataConstructor::BoxedNumber.tag(),
                    Array::from_slice(&view, &[Ref::num(m)]),
                )?
                .as_ptr(),
                machine.root_env(),
            ),
        );
        fields.insert(
            "d".to_string(),
            Closure::new(
                view.data(
                    DataConstructor::BoxedNumber.tag(),
                    Array::from_slice(&view, &[Ref::num(d)]),
                )?
                .as_ptr(),
                machine.root_env(),
            ),
        );
        fields.insert(
            "H".to_string(),
            Closure::new(
                view.data(
                    DataConstructor::BoxedNumber.tag(),
                    Array::from_slice(&view, &[Ref::num(hour)]),
                )?
                .as_ptr(),
                machine.root_env(),
            ),
        );
        fields.insert(
            "M".to_string(),
            Closure::new(
                view.data(
                    DataConstructor::BoxedNumber.tag(),
                    Array::from_slice(&view, &[Ref::num(min)]),
                )?
                .as_ptr(),
                machine.root_env(),
            ),
        );
        fields.insert(
            "S".to_string(),
            Closure::new(
                view.data(
                    DataConstructor::BoxedNumber.tag(),
                    Array::from_slice(&view, &[Ref::num(Number::from_f64(sec).unwrap())]),
                )?
                .as_ptr(),
                machine.root_env(),
            ),
        );
        fields.insert(
            "Z".to_string(),
            Closure::new(
                view.data(
                    DataConstructor::BoxedString.tag(),
                    Array::from_slice(&view, &[view.str_ref(tz)?]),
                )?
                .as_ptr(),
                machine.root_env(),
            ),
        );

        machine_return_block_pair_closure_list(machine, view, fields)
    }
}

impl CallGlobal1 for ZdtFields {}

/// ZDT.FROM_EPOCH - convert a unix timestamp to a zoned date time
pub struct ZdtFromEpoch;

impl StgIntrinsic for ZdtFromEpoch {
    fn name(&self) -> &str {
        "ZDT.FROM_EPOCH"
    }

    /// Simply compose ZDT.FROM_EPOCH and ZDT.FIELDS
    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let unix = num_arg(machine, view, &args[0])?;
        if let Some(ts) = unix.as_i64() {
            let zdt = DateTime::from(Utc.timestamp(ts, 0));
            machine_return_zdt(machine, view, zdt)
        } else {
            Err(ExecutionError::BadTimestamp(unix))
        }
    }
}

impl CallGlobal1 for ZdtFromEpoch {}

/// IFIELDS - compatibility with old API - epoch to fields
pub struct ZdtIFields;

impl StgIntrinsic for ZdtIFields {
    fn name(&self) -> &str {
        "IFIELDS"
    }

    /// The STG wrapper for calling the intrinsic
    fn wrapper(&self, annotation: Smid) -> super::syntax::LambdaForm {
        use super::syntax::dsl::*;
        annotated_lambda(
            1,
            force(ZdtFromEpoch.global(lref(0)), ZdtFields.global(lref(0))),
            annotation,
        )
    }
}

impl CallGlobal1 for ZdtIFields {}

/// ZDT.PARSE - parse an ISO8601 date
pub struct ZdtParse8601;

fn zdt_from_str(repr: &str) -> Option<DateTime<FixedOffset>> {
    // first try parsing a full datetime
    DateTime::parse_from_rfc3339(repr)
        .or_else(|_| DateTime::parse_from_str(repr, "%Y%m%dT%H%M%S"))
        .or_else(|_| {
            repr.parse::<NaiveDateTime>()
                .map(|dt| DateTime::from_utc(dt, FixedOffset::east(0)))
        })
        .or_else(|_| {
            repr.parse::<NaiveDate>()
                .map(|d| DateTime::from_utc(d.and_hms(0, 0, 0), FixedOffset::east(0)))
        })
        .or_else(|_| {
            NaiveDateTime::parse_from_str(repr, "%Y%m%dT%H%M%S")
                .map(|dt| DateTime::from_utc(dt, FixedOffset::east(0)))
        })
        .or_else(|_| {
            NaiveDateTime::parse_from_str(repr, "%Y%m%dT%H%M")
                .map(|dt| DateTime::from_utc(dt, FixedOffset::east(0)))
        })
        .or_else(|_| {
            NaiveDate::parse_from_str(repr, "%Y%m%d")
                .map(|d| DateTime::from_utc(d.and_hms(0, 0, 0), FixedOffset::east(0)))
        })
        .ok()
}

impl StgIntrinsic for ZdtParse8601 {
    fn name(&self) -> &str {
        "ZDT.PARSE"
    }

    /// Simply compose ZDT.FROM_EPOCH and ZDT.FIELDS
    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let repr = str_arg(machine, view, &args[0])?;
        if let Some(zdt) = zdt_from_str(&repr) {
            machine_return_zdt(machine, view, zdt)
        } else {
            Err(ExecutionError::BadDateTimeString(repr))
        }
    }
}

impl CallGlobal1 for ZdtParse8601 {}

/// ZDT.FORMAT - format date as ISO8601
pub struct ZdtFormat8601;

impl StgIntrinsic for ZdtFormat8601 {
    fn name(&self) -> &str {
        "ZDT.FORMAT"
    }

    /// Simply compose ZDT.FROM_EPOCH and ZDT.FIELDS
    fn execute<'guard>(
        &self,
        machine: &mut dyn IntrinsicMachine,
        view: MutatorHeapView<'guard>,
        _emitter: &mut dyn Emitter,
        args: &[Ref],
    ) -> Result<(), ExecutionError> {
        let zdt = zdt_arg(machine, view, &args[0])?;
        let repr = zdt.to_rfc3339();
        machine_return_str(machine, view, repr)
    }
}

impl CallGlobal1 for ZdtFormat8601 {}

#[cfg(test)]
pub mod tests {
    use super::*;
    use chrono::FixedOffset;

    #[test]
    pub fn test_tz_parse() {
        assert_eq!(
            offset_from_tz_str("-02:00").unwrap(),
            FixedOffset::east(2 * 60 * 60)
        );
        assert_eq!(
            offset_from_tz_str("+02:00").unwrap(),
            FixedOffset::west(2 * 60 * 60)
        );
        assert_eq!(offset_from_tz_str("UTC").unwrap(), FixedOffset::east(0));
        assert_eq!(offset_from_tz_str("Z").unwrap(), FixedOffset::east(0));
    }
}
