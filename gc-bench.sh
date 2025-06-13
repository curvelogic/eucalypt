#!/bin/bash
set -e

# GC-focused performance monitoring script
# Simplified workflow for checking GC impact during development

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}GC Performance Monitor${NC}"
echo "====================="

# Function to run GC tests and show memory stats
run_gc_tests() {
    echo -e "\n${YELLOW}Running GC test suite...${NC}"
    cargo test --test harness_test test_gc --quiet
    echo -e "${GREEN}✓ GC tests passed${NC}"
}

# Function to run allocation-focused benchmarks
run_allocation_bench() {
    echo -e "\n${YELLOW}Running allocation benchmarks...${NC}"
    cargo bench --bench alloc -- --measurement-time 2 --warm-up-time 1 --quiet
    echo -e "${GREEN}✓ Allocation benchmarks completed${NC}"
}

# Function to check for performance regressions
performance_check() {
    echo -e "\n${BLUE}Performance Check Workflow${NC}"
    echo "1. Running GC tests to ensure correctness..."
    run_gc_tests
    
    echo -e "\n2. Running allocation benchmarks..."
    ./bench.sh quick
    
    echo -e "\n${GREEN}✓ Performance check complete${NC}"
    echo "If any benchmarks show significant regressions, investigate before continuing GC work."
}

# Function to establish baseline before GC changes
establish_baseline() {
    local name="${1:-before_gc_changes}"
    echo -e "\n${BLUE}Establishing baseline: $name${NC}"
    
    echo "1. Running full test suite..."
    cargo test --lib --quiet
    
    echo -e "\n2. Saving performance baseline..."
    ./bench.sh baseline "$name"
    
    echo -e "\n${GREEN}✓ Baseline established: $name${NC}"
    echo "You can now make GC changes and use './gc-bench.sh check' to monitor impact."
}

# Function to show GC-related benchmark history
show_gc_history() {
    echo -e "\n${BLUE}GC Benchmark History${NC}"
    
    if [ -d "benchmark_results" ]; then
        echo "Recent benchmark runs:"
        ls -la benchmark_results/*.log 2>/dev/null | tail -10
        
        echo -e "\nAvailable baselines:"
        ./bench.sh list
    else
        echo "No benchmark history found. Run 'establish' to create baseline."
    fi
}

case "${1:-help}" in
    "check"|"c")
        performance_check
        ;;
    
    "establish"|"e")
        establish_baseline "$2"
        ;;
    
    "baseline"|"b")
        establish_baseline "$2"
        ;;
    
    "tests"|"t")
        run_gc_tests
        ;;
    
    "bench"|"alloc")
        run_allocation_bench
        ;;
    
    "history"|"h")
        show_gc_history
        ;;
    
    "help"|*)
        echo "Usage: $0 <command> [name]"
        echo ""
        echo "Commands:"
        echo "  check (c)            Quick performance check (tests + benchmark vs baseline)"
        echo "  establish (e) [name] Establish performance baseline before GC changes"
        echo "  baseline (b) [name]  Alias for establish"
        echo "  tests (t)            Run GC test suite only"
        echo "  bench                Run allocation benchmarks only"
        echo "  history (h)          Show benchmark history"
        echo "  help                 Show this help"
        echo ""
        echo "Typical workflow:"
        echo "  1. $0 establish               # Before starting GC work"
        echo "  2. # Make GC changes..."
        echo "  3. $0 check                   # After each significant change"
        echo ""
        echo "Quick commands for development:"
        echo "  $0 c     # Quick check"
        echo "  $0 t     # Just tests" 
        echo "  $0 h     # Show history"
        ;;
esac