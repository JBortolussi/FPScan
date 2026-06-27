import os
import shutil

from functools import cmp_to_key

from .table import Table, bf
from .tiny import Tiny

from .fpcore_prgm import get_fpcore_prgm

from .fpchecker.sample import Sample

from .fpchecker.report_generator import generate_report as generate_fpc_report
from .tiny.report_generator import generate_report as generate_tiny_report

BOT = "$\\bot$"

N_point = 1000

def compare(
    prgm_dir: str = "fpcore_prgm",
    extension: str= ".fpcore",
    report_root_dir: str= "report",
    timeout: int = 420,
    N_point: int = 1000,
    # pdf_path: str,
    rebuild_fpchecker: bool = False,
    rebuild_fpscan: bool = False,
    rebuild_bitblasting: bool = False,
    log: bool = False
) -> None:
    prgm_dir = os.path.abspath(prgm_dir)
    report_root_dir = os.path.abspath(report_root_dir)
    # pdf_path = os.path.abspath(pdf_path)

    prgm_name_set, trigo_prgm_name_set = get_fpcore_prgm()
    prgm_name_set = set(map(lambda name: name.replace(extension, ""), prgm_name_set))
    trigo_prgm_name_set = set(map(lambda name: name.replace(extension, ""), trigo_prgm_name_set))

    run_tiny = Tiny("./fpscan.exe", timeout=timeout)

    prgm_name_list = list(prgm_name_set)
    prgm_name_no_trigo_list = list(prgm_name_set - trigo_prgm_name_set)


    # generate bitblast report
    print("Generating bitblast report")
    run_tiny.set_opt(["-bitblasting"])
    bitblast_report_dict = generate_tiny_report(
        prgm_dir,
        prgm_name_no_trigo_list,
        extension,
        run_tiny,
        os.path.join(report_root_dir, "report_bitblast"),
        log=log,
        force_rebuild=rebuild_bitblasting,
    )

    # generate standard report
    print("Generating FPScan report")
    run_tiny.set_opt([])
    fpscan_report_dict = generate_tiny_report(
        prgm_dir,
        prgm_name_list,
        extension,
        run_tiny,
        os.path.join(report_root_dir, "report"),
        log=log,
        force_rebuild=rebuild_fpscan,
    )

    # generate fpc report
    print("Generating FPChecker report")
    fpc_report_dict = generate_fpc_report(
        prgm_dir,
        prgm_name_list,
        extension,
        Sample(),
        os.path.join(report_root_dir, f"report_fpc_{N_point}"),
        N_point,
        keep_working_dir=False,
        force_rebuild=rebuild_fpchecker,
        log=log
    )

    # print("Create table")
    # t = Table("Results of absorption and catastrophic cancellation detection by FPChecker \\cite{laguna_fpchecker_2019,laguna_fpchecker_2022}, Bit-blasting, and FPScan.", [
    #     ["\\textbf{Program}", "l"],
    #     "\\textbf{Max}", 
    #     ["\\textbf{FPChecker}", "r"],
    #     ["\\textbf{Bitblasting}", "c"],
    #     ["\\textbf{FPScan}", "l"],
    #     "\\textbf{Max}", 
    #     ["\\textbf{Bit-blasting}", "r"],
    #     ["\\textbf{FPScan}", "l"],
    # ])

    # t.add_top_header("\\textbf{Cancellation}", 1, 4)
    # t.add_top_header("\\textbf{Absorption}", 5, 3)

    fd = open("test.csv", "w")

    max_op = 0
    min_op = 1000
    used_prgm = 0
    for name, std_r in fpscan_report_dict.items():
        fpc_r = fpc_report_dict[name]

        M = std_r.nb_potential_pitfall
        M_cancellation = M // 3
        M_absorption = M_cancellation * 2

        fpc_cancellation = fpc_r.get_cancellation()
        fpc_cancellation_count = len(fpc_cancellation)

        std_absorption = std_r.get_absorption()
        std_absorption_count = len(std_absorption)
        std_cancellation = std_r.get_cancellation()
        std_cancellation_count = len(std_cancellation)
        std_pitfall_count = std_absorption_count + std_cancellation_count

        if name not in bitblast_report_dict or bitblast_report_dict[name].time is None:
            continue
            bitblast_absorption_count: str | int = BOT
            bitblast_cancellation_count: str | int = M_cancellation
            bitblast_absorption = None
            bitblast_cancellation = None
            bitblast_pitfall_count = 0
            fpc_soundness: float | str = BOT
            std_precision: float | str = BOT
        else:
            used_prgm += 1
            n_op = std_r.nb_op
            if n_op > max_op:
                max_op = n_op
            if n_op < min_op:
                min_op = n_op

            if (log):
                print(f"{name}: {n_op}")

            bitblast_r = bitblast_report_dict[name]
            bitblast_absorption = bitblast_r.get_absorption()
            bitblast_absorption_count = len(bitblast_absorption)
            bitblast_cancellation = bitblast_r.get_cancellation()
            bitblast_cancellation_count = len(bitblast_cancellation)
            bitblast_pitfall_count = bitblast_absorption_count + bitblast_cancellation_count

            fpc_false_negatives = bitblast_cancellation - fpc_cancellation
            fpc_false_negatives_count: int = len(fpc_false_negatives)

            if bitblast_cancellation_count == 0:
                fpc_soundness = 1.0
            else:
                fpc_soundness = (bitblast_cancellation_count - fpc_false_negatives_count) / bitblast_cancellation_count

            std_false_positives = (std_absorption | std_cancellation) - (bitblast_absorption | bitblast_cancellation)
            std_false_positives_count: int | str = len(std_false_positives)
            
            if std_pitfall_count == 0:
                std_precision = 1.0
            else:
                std_precision = (std_pitfall_count - std_false_positives_count) / std_pitfall_count
        # t.add_line([
        #     name,
        #     M_cancellation,
        #     fpc_cancellation_count,
        #     bitblast_cancellation_count,
        #     std_cancellation_count,
        #     M_absorption,
        #     bitblast_absorption_count,
        #     std_absorption_count,
        # ])
        fd.write(f"{name}; {M_cancellation-std_cancellation_count}; {std_cancellation_count}; {fpc_cancellation_count}; {bitblast_cancellation_count - fpc_cancellation_count}\n")

    # print("\n".join(prgm_name_no_trigo_list))
    print(f"Number of analyzed programs: {len(fpscan_report_dict)}")
    print(f"Number of program used: {used_prgm}")
    print(f"max_op: {max_op}")
    print(f"min_op: {min_op}")
    fd.close()
    # # x < y
    # def cmp(x, y):
    #     x = x.get_val(3)
    #     y = y.get_val(3)
    #     if x == BOT:
    #         return 1
    #     elif y == BOT:
    #         return -1
    #     elif x > y:
    #         return 1
    #     elif x == y:
    #         return 0
    #     else:
    #         return -1

    # t.sort(cmp_to_key(cmp))

    # format_fun = lambda d: "\\textbf{" + d + "}"
    # for line in t.lines:
    #     color_midle = False
    #     v = line.get_val(3)
    #     if line.get_val(2) == v:
    #         color_midle = True
    #         line.set_second_col_formatter(2, format_fun)
    #     if line.get_val(4) == v:
    #         color_midle = True
    #         line.set_second_col_formatter(4, format_fun)
    #     if color_midle:
    #         line.set_second_col_formatter(3, format_fun)

    #     color_midle = False
    #     if line.get_val(6) == line.get_val(7):
    #         line.set_second_col_formatter(6, format_fun)
    #         line.set_second_col_formatter(7, format_fun)


    # t.build_pdf(
    #     latex_dir=os.path.join("tex", f"tex_fpc_{N_point}"),
    #     pdf_path=pdf_path,
    # )