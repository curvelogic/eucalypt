// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="welcome/index.html">Welcome</a></li><li class="chapter-item expanded affix "><li class="part-title">Getting Started</li><li class="chapter-item expanded "><a href="welcome/what-is-eucalypt.html"><strong aria-hidden="true">1.</strong> What is Eucalypt?</a></li><li class="chapter-item expanded "><a href="welcome/quick-start.html"><strong aria-hidden="true">2.</strong> Quick Start</a></li><li class="chapter-item expanded "><a href="welcome/by-example.html"><strong aria-hidden="true">3.</strong> Eucalypt by Example</a></li><li class="chapter-item expanded affix "><li class="part-title">The Eucalypt Guide</li><li class="chapter-item expanded "><a href="guide/blocks-and-declarations.html"><strong aria-hidden="true">4.</strong> Blocks and Declarations</a></li><li class="chapter-item expanded "><a href="guide/expressions-and-pipelines.html"><strong aria-hidden="true">5.</strong> Expressions and Pipelines</a></li><li class="chapter-item expanded "><a href="guide/lists-and-transformations.html"><strong aria-hidden="true">6.</strong> Lists and Transformations</a></li><li class="chapter-item expanded "><a href="guide/string-interpolation.html"><strong aria-hidden="true">7.</strong> Strings and Text</a></li><li class="chapter-item expanded "><a href="guide/functions-and-combinators.html"><strong aria-hidden="true">8.</strong> Functions and Combinators</a></li><li class="chapter-item expanded "><a href="guide/operators.html"><strong aria-hidden="true">9.</strong> Operators</a></li><li class="chapter-item expanded "><a href="guide/anaphora.html"><strong aria-hidden="true">10.</strong> Anaphora (Implicit Parameters)</a></li><li class="chapter-item expanded "><a href="guide/block-manipulation.html"><strong aria-hidden="true">11.</strong> Block Manipulation</a></li><li class="chapter-item expanded "><a href="guide/navigating-nested-data.html"><strong aria-hidden="true">12.</strong> Navigating Nested Data</a></li><li class="chapter-item expanded "><a href="guide/imports-and-modules.html"><strong aria-hidden="true">13.</strong> Imports and Modules</a></li><li class="chapter-item expanded "><a href="guide/working-with-data.html"><strong aria-hidden="true">14.</strong> Working with Data</a></li><li class="chapter-item expanded "><a href="guide/command-line.html"><strong aria-hidden="true">15.</strong> The Command Line</a></li><li class="chapter-item expanded "><a href="guide/yaml-embedding.html"><strong aria-hidden="true">16.</strong> YAML Embedding</a></li><li class="chapter-item expanded "><a href="guide/testing.html"><strong aria-hidden="true">17.</strong> Testing with Eucalypt</a></li><li class="chapter-item expanded "><a href="guide/date-time-random.html"><strong aria-hidden="true">18.</strong> Date, Time, and Random Numbers</a></li><li class="chapter-item expanded "><a href="guide/io.html"><strong aria-hidden="true">19.</strong> IO and Shell Commands</a></li><li class="chapter-item expanded "><a href="guide/monads.html"><strong aria-hidden="true">20.</strong> Monads and the monad() Utility</a></li><li class="chapter-item expanded "><a href="guide/state-monad.html"><strong aria-hidden="true">21.</strong> The State Monad</a></li><li class="chapter-item expanded "><a href="guide/type-checking.html"><strong aria-hidden="true">22.</strong> Type Checking</a></li><li class="chapter-item expanded "><a href="guide/advanced-topics.html"><strong aria-hidden="true">23.</strong> Advanced Topics</a></li><li class="chapter-item expanded affix "><li class="part-title">Reference</li><li class="chapter-item expanded "><a href="reference/syntax.html"><strong aria-hidden="true">24.</strong> Language Syntax Reference</a></li><li class="chapter-item expanded "><a href="reference/operators-and-identifiers.html"><strong aria-hidden="true">25.</strong> Operator Precedence Table</a></li><li class="chapter-item expanded "><a href="reference/prelude/index.html"><strong aria-hidden="true">26.</strong> Prelude Reference</a><a class="toggle"><div>❱</div></a></li><li><ol class="section"><li class="chapter-item "><a href="reference/prelude/lists.html"><strong aria-hidden="true">26.1.</strong> Lists</a></li><li class="chapter-item "><a href="reference/prelude/blocks.html"><strong aria-hidden="true">26.2.</strong> Blocks</a></li><li class="chapter-item "><a href="reference/prelude/strings.html"><strong aria-hidden="true">26.3.</strong> Strings</a></li><li class="chapter-item "><a href="reference/prelude/numbers.html"><strong aria-hidden="true">26.4.</strong> Numbers and Arithmetic</a></li><li class="chapter-item "><a href="reference/prelude/booleans.html"><strong aria-hidden="true">26.5.</strong> Booleans and Comparison</a></li><li class="chapter-item "><a href="reference/prelude/combinators.html"><strong aria-hidden="true">26.6.</strong> Combinators</a></li><li class="chapter-item "><a href="reference/prelude/calendar.html"><strong aria-hidden="true">26.7.</strong> Calendar</a></li><li class="chapter-item "><a href="reference/prelude/sets.html"><strong aria-hidden="true">26.8.</strong> Sets</a></li><li class="chapter-item "><a href="reference/prelude/random.html"><strong aria-hidden="true">26.9.</strong> Random Numbers</a></li><li class="chapter-item "><a href="reference/prelude/metadata.html"><strong aria-hidden="true">26.10.</strong> Metadata</a></li><li class="chapter-item "><a href="reference/prelude/io.html"><strong aria-hidden="true">26.11.</strong> IO</a></li><li class="chapter-item "><a href="reference/prelude/arrays.html"><strong aria-hidden="true">26.12.</strong> N-Dimensional Arrays</a></li></ol></li><li class="chapter-item expanded "><a href="reference/cli.html"><strong aria-hidden="true">27.</strong> CLI Reference</a></li><li class="chapter-item expanded "><a href="reference/import-formats.html"><strong aria-hidden="true">28.</strong> Import Formats</a></li><li class="chapter-item expanded "><a href="reference/export-formats.html"><strong aria-hidden="true">29.</strong> Export Formats</a></li><li class="chapter-item expanded "><a href="reference/error-messages.html"><strong aria-hidden="true">30.</strong> Error Messages Guide</a></li><li class="chapter-item expanded "><a href="reference/agent-reference.html"><strong aria-hidden="true">31.</strong> Agent Reference</a></li><li class="chapter-item expanded affix "><li class="part-title">Understanding Eucalypt</li><li class="chapter-item expanded "><a href="understanding/philosophy.html"><strong aria-hidden="true">32.</strong> Design Philosophy</a></li><li class="chapter-item expanded "><a href="understanding/lazy-evaluation.html"><strong aria-hidden="true">33.</strong> Lazy Evaluation</a></li><li class="chapter-item expanded "><a href="architecture.html"><strong aria-hidden="true">34.</strong> Architecture</a></li><li class="chapter-item expanded "><a href="eucalypt-style.html"><strong aria-hidden="true">35.</strong> Eucalypt Style</a></li><li class="chapter-item expanded affix "><li class="part-title">FAQ</li><li class="chapter-item expanded "><a href="faq.html"><strong aria-hidden="true">36.</strong> Frequently Asked Questions</a></li><li class="chapter-item expanded affix "><li class="part-title">Appendices</li><li class="chapter-item expanded "><a href="appendices/cheat-sheet.html"><strong aria-hidden="true">37.</strong> Syntax Cheat Sheet</a></li><li class="chapter-item expanded "><a href="appendices/syntax-gotchas.html"><strong aria-hidden="true">38.</strong> Syntax Gotchas</a></li><li class="chapter-item expanded "><a href="appendices/migration.html"><strong aria-hidden="true">39.</strong> Migration from v0.2 to v0.3</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
