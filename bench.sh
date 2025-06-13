#!/bin/bash
set -e

# Simple local benchmarking script for Eucalypt GC development
# This provides reliable performance monitoring without CI overhead

BENCHMARK_NAME="gc_benchmark"
BASELINE_NAME="baseline"
RESULTS_DIR="benchmark_results"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Eucalypt GC Performance Benchmark${NC}"
echo "================================="

# Create results directory if it doesn't exist
mkdir -p "$RESULTS_DIR"

# Function to run benchmark and save results
run_benchmark() {
    local name="$1"
    local description="$2"
    
    echo -e "\n${YELLOW}Running benchmark: $name${NC}"
    echo "Description: $description"
    
    # Run the benchmark with much shorter measurement time for faster feedback
    cargo bench --bench alloc -- --measurement-time 3 --warm-up-time 1 --save-baseline "$name" > "${RESULTS_DIR}/${name}.log" 2>&1
    
    echo -e "${GREEN}✓ Benchmark completed: $name${NC}"
    echo "Results saved to: ${RESULTS_DIR}/${name}.log"
}

# Function to compare benchmarks
compare_benchmarks() {
    local baseline="$1"
    local current="$2"
    
    echo -e "\n${YELLOW}Comparing $current vs $baseline${NC}"
    
    # Run comparison benchmark
    cargo bench --bench alloc -- --measurement-time 3 --warm-up-time 1 --baseline "$baseline" > "${RESULTS_DIR}/comparison_${current}_vs_${baseline}.log" 2>&1
    
    echo -e "${GREEN}✓ Comparison completed${NC}"
    echo "Results saved to: ${RESULTS_DIR}/comparison_${current}_vs_${baseline}.log"
    
    # Show key results
    echo -e "\n${BLUE}Performance Summary:${NC}"
    grep -A 3 -B 1 "change:" "${RESULTS_DIR}/comparison_${current}_vs_${baseline}.log" || echo "No significant changes detected"
}

# Function to show available baselines
list_baselines() {
    echo -e "\n${BLUE}Available baselines:${NC}"
    if [ -d "target/criterion" ]; then
        find target/criterion -name "*.json" -path "*/*/estimates.json" | sed 's|target/criterion/||; s|/.*/estimates.json||' | sort | uniq
    else
        echo "No baselines found. Run with --save-baseline first."
    fi
}

# Function to show benchmark trends
show_trends() {
    echo -e "\n${BLUE}Recent benchmark logs:${NC}"
    ls -la "$RESULTS_DIR"/*.log 2>/dev/null || echo "No previous benchmark results found"
}

# Parse command line arguments
case "${1:-help}" in
    "baseline"|"save-baseline")
        BASELINE_NAME="${2:-$BASELINE_NAME}"
        run_benchmark "$BASELINE_NAME" "Saving baseline performance for comparison"
        echo -e "\n${GREEN}Baseline '$BASELINE_NAME' saved!${NC}"
        echo "Use: ./bench.sh compare <name> $BASELINE_NAME"
        ;;
    
    "run")
        BENCHMARK_NAME="${2:-$BENCHMARK_NAME}"
        run_benchmark "$BENCHMARK_NAME" "Current performance benchmark"
        ;;
    
    "compare")
        CURRENT="${2:-current}"
        BASELINE="${3:-$BASELINE_NAME}"
        
        if [ "$CURRENT" = "$BASELINE" ]; then
            echo -e "${RED}Error: Cannot compare baseline to itself${NC}"
            exit 1
        fi
        
        # First run current benchmark
        run_benchmark "$CURRENT" "Running current code for comparison"
        
        # Then compare against baseline
        compare_benchmarks "$BASELINE" "$CURRENT"
        ;;
    
    "quick")
        # Quick performance check - save baseline if none exists, otherwise compare
        if [ ! -d "target/criterion" ] || [ -z "$(find target/criterion -name "*.json" 2>/dev/null)" ]; then
            echo "No baseline found. Creating initial baseline..."
            run_benchmark "$BASELINE_NAME" "Initial baseline"
        else
            TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
            run_benchmark "quick_$TIMESTAMP" "Quick performance check"
            compare_benchmarks "$BASELINE_NAME" "quick_$TIMESTAMP"
        fi
        ;;
    
    "list")
        list_baselines
        ;;
    
    "trends")
        show_trends
        ;;
    
    "clean")
        echo -e "${YELLOW}Cleaning benchmark data...${NC}"
        rm -rf target/criterion
        rm -rf "$RESULTS_DIR"
        echo -e "${GREEN}✓ Benchmark data cleaned${NC}"
        ;;
    
    "help"|*)
        echo "Usage: $0 <command> [arguments]"
        echo ""
        echo "Commands:"
        echo "  baseline [name]          Save current performance as baseline (default: baseline)"
        echo "  run [name]              Run benchmark with given name (default: gc_benchmark)"
        echo "  compare <current> <baseline>  Compare current vs baseline performance"
        echo "  quick                   Quick check: save baseline if none exists, otherwise compare to baseline"
        echo "  list                    List available benchmark baselines"
        echo "  trends                  Show recent benchmark results"
        echo "  clean                   Remove all benchmark data"
        echo "  help                    Show this help"
        echo ""
        echo "Examples:"
        echo "  $0 baseline                    # Save current performance as baseline"
        echo "  $0 baseline before_gc_changes  # Save named baseline"
        echo "  $0 quick                       # Quick performance check"
        echo "  $0 compare after_gc before_gc  # Compare two benchmarks"
        echo ""
        echo "Tip: Run 'baseline' before making GC changes, then 'quick' to check impact!"
        ;;
esac