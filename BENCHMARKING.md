# Eucalypt Performance Benchmarking

Simple and reliable local benchmarking that replaces problematic CI/CD benchmarks.

## Why Local Benchmarking?

The previous GitHub Actions benchmark job was unreliable:
- ❌ Resource limits caused SIGKILL failures
- ❌ 60+ second measurements were too slow for development
- ❌ Network and environment variability
- ❌ Limited GitHub Actions runtime

Local benchmarking solves these issues with:
- ✅ Consistent local environment
- ✅ Fast 3-second measurements  
- ✅ No resource limits
- ✅ Immediate feedback during development

## Quick Start

```bash
# 1. Establish baseline before making GC changes
./gc-bench.sh establish

# 2. Make your GC changes...

# 3. Quick performance check after changes
./gc-bench.sh check

# 4. Show history
./gc-bench.sh history
```

## Scripts

### `gc-bench.sh` - GC Development Workflow
Streamlined script for GC development:
- `establish [name]` - Save baseline before GC work
- `check` - Quick test + benchmark check
- `tests` - Run GC tests only
- `history` - Show benchmark history

### `bench.sh` - Detailed Benchmarking  
Full benchmarking control:
- `baseline [name]` - Save named baseline
- `run [name]` - Run named benchmark
- `compare <current> <baseline>` - Compare performance
- `quick` - Auto-compare vs baseline
- `list` - Show available baselines

## Key Benefits

✅ **Fast**: 3-second measurements instead of CI's 60+ seconds  
✅ **Local**: No CI resource limits or SIGKILL issues  
✅ **Reliable**: Consistent environment, no network dependencies  
✅ **Simple**: One-command performance checks  
✅ **Historical**: Track performance trends over time  

## Benchmark Coverage

Tests core allocation patterns:
- `alloc_let` - Let binding allocation
- `alloc_letrec` - Recursive binding allocation  
- `deep_env_access` - Environment traversal
- `deep_env_update` - Environment updates
- `create_and_saturate_lambda` - Lambda allocation/application
- `create_partially_apply_and_saturate_lambda` - Partial application

## Performance Monitoring Strategy

1. **Before major GC changes**: `./gc-bench.sh establish pre_change_name`
2. **During development**: `./gc-bench.sh check` frequently  
3. **Performance regression detected**: Investigate before continuing
4. **Major milestone**: `./gc-bench.sh establish milestone_name`

## Reading Results

- **No change detected**: Good! (p > 0.05)
- **Change within noise threshold**: Acceptable variation (< 5%)  
- **Significant change**: Review if acceptable for your changes
- **Large regressions**: Investigate immediately

## Files

- `benchmark_results/` - Log files from benchmark runs
- `target/criterion/` - Criterion's detailed benchmark data
- Both directories are gitignored to avoid repository bloat

## Example Output

```
alloc_let               time:   [142.28 ns 143.46 ns 144.72 ns]
                        change: [+0.4631% +2.3701% +4.3031%] (p = 0.02 < 0.05)
                        Change within noise threshold.
```

This shows a 2.37% increase (within noise threshold) in let allocation time.

## Troubleshooting

**Benchmarks take too long**: Already optimized to 3-second measurements  
**Inconsistent results**: Run multiple times, criterion averages 100 samples  
**No baseline error**: Run `./gc-bench.sh establish` first  
**Comparison issues**: Use `./bench.sh list` to see available baselines