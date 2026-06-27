from .mk_tps_plot import mk_tps_plot
from .compare_fpc_bit_std import compare as mk_table
from .run_fpchecker import run_fpchecker
from .compare_fps_bit import mk_tables as compare_fps_bit
from .timeout_plot import make_plot as make_timeout_plot
from .cmp_fps_fpc import compare_fps_fpc

import argparse

parser = argparse.ArgumentParser()

subparsers = parser.add_subparsers(dest="subcommand")
tps_plot_parser = subparsers.add_parser('tps', help="Build execution time figure")
tps_plot_parser.add_argument("--prgm_dir", help="path to the directory containing test programs", default="fpcore_prgm")
tps_plot_parser.add_argument("--report_dir", help="save the report in that directory", default="report")
tps_plot_parser.add_argument("--timeout", help="timeout for bitblasting", default=420, type=int)
tps_plot_parser.add_argument("--N", help="Number of samples used by fpchecker", default=1000, type=int)
tps_plot_parser.add_argument("--save", help="save the figure", action="store_true")
tps_plot_parser.add_argument("--lines", help="line plots, defaults is bar", action="store_true")
tps_plot_parser.add_argument("--rebuild_fpscan", action="store_true", help="Force rebuild of FPScan reports")
tps_plot_parser.add_argument("--rebuild_fpchecker", action="store_true", help="Force rebuild of FPChecker reports")
tps_plot_parser.add_argument("--rebuild_bitblasting", action="store_true", help="Force rebuild of bitblasting reports")
tps_plot_parser.add_argument("--log", action="store_true")

# table_parser = subparsers.add_parser('table', help="Build table figure")
# table_parser.add_argument("--prgm_dir", help="path to the directory containing test programs", default="fpcore_prgm")
# table_parser.add_argument("--report_dir", help="save the report in that directory", default="report")
# table_parser.add_argument("--timeout", help="timeout for bitblasting", default=420, type=int)
# table_parser.add_argument("--N", help="Number of samples used by fpchecker", default=1000, type=int)
# table_parser.add_argument("--rebuild_fpscan", action="store_true", help="Force rebuild of FPScan reports")
# table_parser.add_argument("--rebuild_fpchecker", action="store_true", help="Force rebuild of FPChecker reports")
# table_parser.add_argument("--rebuild_bitblasting", action="store_true", help="Force rebuild of bitblasting reports")
# table_parser.add_argument("--log", action="store_true")

# fpc_run_parser = subparsers.add_parser('fpc', help="Run FPChecker")
# fpc_run_parser.add_argument("prgm_path", help="Path to the program to be analyzed")
# fpc_run_parser.add_argument("--N_point", type=int, default=5000, help="Number of samples")
# fpc_run_parser.add_argument("--rand", action="store_true", help="Random sampling instead of regular")
# fpc_run_parser.add_argument("--print_safe", action="store_true", help="Also print safe points")
# fpc_run_parser.add_argument("--save", action="store_true", help="Save the figure")

fpscan_eval_parser = subparsers.add_parser("fpscan", help="Build FPScan VS Bitblasting table")
fpscan_eval_parser.add_argument("--prgm_dir", help="path to the directory containing test programs", default="fpcore_prgm")
fpscan_eval_parser.add_argument("--fpscan_prgm_dir", help="path to custom test prgm", default="fpscsan")
fpscan_eval_parser.add_argument("--report_dir", help="save the report in that directory", default="report")
fpscan_eval_parser.add_argument("--timeout", help="timeout for bitblasting", default=420, type=int)
fpscan_eval_parser.add_argument("--rebuild_fpscan", action="store_true", help="Force rebuild of FPScan reports")
fpscan_eval_parser.add_argument("--rebuild_bitblasting", action="store_true", help="Force rebuild of bitblasting reports")
fpscan_eval_parser.add_argument("--N", help="Number of samples used by fpchecker", default=1000, type=int)
fpscan_eval_parser.add_argument("--save", action="store_true", help="Save the figure")
fpscan_eval_parser.add_argument("--log", action="store_true")

fpchecker_cmp_parser = subparsers.add_parser("fps_fpc", help="Build figure comparing FPScan and FPChecker")
fpchecker_cmp_parser.add_argument("--prgm_dir", help="path to the directory containing test programs", default="fpcore_prgm")
fpchecker_cmp_parser.add_argument("--fpscan_prgm_dir", help="path to custom test prgm", default="fpscsan")
fpchecker_cmp_parser.add_argument("--report_dir", help="save the report in that directory", default="report")
fpchecker_cmp_parser.add_argument("--timeout", help="timeout for bitblasting", default=420, type=int)
fpchecker_cmp_parser.add_argument("--rebuild_fpchecker", action="store_true", help="Force rebuild of FPChecker reports")
fpchecker_cmp_parser.add_argument("--rebuild_fpscan", action="store_true", help="Force rebuild of FPScan reports")
fpchecker_cmp_parser.add_argument("--rebuild_bitblasting", action="store_true", help="Force rebuild of bitblasting reports")
fpchecker_cmp_parser.add_argument("--N", help="Number of samples used by fpchecker", default=1000, type=int)
fpchecker_cmp_parser.add_argument("--save", action="store_true", help="Save the figure")
fpchecker_cmp_parser.add_argument("--log", action="store_true")

# timeout_plot_parser = subparsers.add_parser("timeout", help="Build FPScan VS Bitblasting table")
# timeout_plot_parser.add_argument("--timeout", help="timeout for bitblasting", default=420, type=int)
# timeout_plot_parser.add_argument("--N", help="Number of samples used by fpchecker", default=1000, type=int)
# timeout_plot_parser.add_argument("--save", action="store_true", help="Save the figure")
# timeout_plot_parser.add_argument("--log", action="store_true")
# timeout_plot_parser.add_argument("--rebuild_fpscan", action="store_true", help="Force rebuild of FPScan reports")
# timeout_plot_parser.add_argument("--rebuild_fpchecker", action="store_true", help="Force rebuild of FPChecker reports")
# timeout_plot_parser.add_argument("--rebuild_bitblasting", action="store_true", help="Force rebuild of bitblasting reports")


args = parser.parse_args()

if args.subcommand == "tps":
    mk_tps_plot(
        args.prgm_dir,
        ".fpcore",
        args.report_dir,
        args.timeout,
        args.N,
        args.save,
        bar=not args.lines,
        rebuild_fpscan=args.rebuild_fpscan,
        rebuild_fpchecker=args.rebuild_fpchecker,
        rebuild_bitblasting=args.rebuild_bitblasting,
        log=args.log,
    )
elif args.subcommand == "table":
    mk_table(
        prgm_dir=args.prgm_dir,
        extension=".fpcore",
        report_root_dir=args.report_dir,
        timeout=args.timeout,
        N_point=args.N,
        pdf_path="table.pdf",
        rebuild_fpscan=args.rebuild_fpscan,
        rebuild_fpchecker=args.rebuild_fpchecker,
        rebuild_bitblasting=args.rebuild_bitblasting,
        log=args.log
    )
elif args.subcommand == "fpc":
    run_fpchecker(
        prgm_path=args.prgm_path,
        N_point=args.N_point,
        random_sample=args.rand,
        print_safe_point=args.print_safe,
        save=args.save,
    )
elif args.subcommand == "fpscan":
    compare_fps_bit(
        report_root_dir=args.report_dir,
        timeout=args.timeout,
        rebuild_fpscan=args.rebuild_fpscan,
        rebuild_bitblasting=args.rebuild_bitblasting,
        log=args.log
    )
elif args.subcommand == "timeout":
    make_timeout_plot(
        timeout=args.timeout,
        N_point=args.N,
        save=args.save,
        rebuild_fpscan=args.rebuild_fpscan,
        rebuild_fpchecker=args.rebuild_fpchecker,
        rebuild_bitblasting=args.rebuild_bitblasting,
        log=args.log
    )
elif args.subcommand == "fps_fpc":
    compare_fps_fpc(
        prgm_dir=args.prgm_dir,
        extension=".fpcore",
        report_root_dir=args.report_dir,
        timeout=args.timeout,
        N_point=args.N,
        rebuild_fpscan=args.rebuild_fpscan,
        rebuild_fpchecker=args.rebuild_fpchecker,
        rebuild_bitblasting=args.rebuild_bitblasting,
        log=args.log
    )