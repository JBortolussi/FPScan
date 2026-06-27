import os

from .fpcore_prgm import get_fpcore_prgm, mk_fpcore_path, fpcore_dir
from .herbie import run_herbie
from .report import Report
from .table import Table
from .tiny import Tiny
from .tiny.report_generator import generate_report as generate_tiny_report

HERBIE_DIR = "herbie_prgm"
extension = ".fpcore"
timeout = 420
report_root_dir = "report"
rebuild_fpscan = False
log = False

skip = {
    "nonlin1.fpcore",
    "intro_example_mixed.fpcore"
}

prgm_name_set, trigo_prgm_name_set = get_fpcore_prgm()
prgm_name_set = prgm_name_set - skip
prgm_name_set = set(map(lambda name: name.replace(extension, ""), prgm_name_set))
trigo_prgm_name_set = set(map(lambda name: name.replace(extension, ""), trigo_prgm_name_set))

run_tiny = Tiny("./fpscan.exe", timeout=timeout)

prgm_name_list = list(prgm_name_set)
prgm_name_no_trigo_list = list(prgm_name_set - trigo_prgm_name_set)

# generate herbie prgm
print("Generate Herbie prgm")
for prgm_name in prgm_name_list:
    prgm_full_name = prgm_name + extension
    prgm_path = mk_fpcore_path(prgm_full_name)
    herbie_path = os.path.join(HERBIE_DIR, prgm_full_name)

    run_herbie(prgm_path, herbie_path, log=False)

# add case_study
prgm_full_name = "case_study.fpcore"
prgm_path = os.path.join("case_study_prgm", prgm_full_name)
herbie_path = os.path.join(HERBIE_DIR, prgm_full_name)
run_herbie(prgm_path, herbie_path, log=False)

# generate original report
print("Generate FPScan reports")
run_tiny.set_opt([])
fpscan_report_dict = generate_tiny_report(
    fpcore_dir,
    prgm_name_list,
    extension,
    run_tiny,
    os.path.join(report_root_dir, "report"),
    log=log,
    force_rebuild=rebuild_fpscan,
)
# handle case study
run_tiny.set_opt([])
herbie_report_path = os.path.join(report_root_dir, "report_herbie")
r = Report.build(
    "case_study",
    prgm_path,
    herbie_report_path,
    run_tiny=run_tiny,
    force_rebuild=rebuild_fpscan
)
fpscan_report_dict["case_study"] = r


# generate Herbie report

herbie_report_dict = generate_tiny_report(
    HERBIE_DIR,
    prgm_name_list + ["case_study"],
    extension,
    run_tiny,
    herbie_report_path,
    log=True,
    force_rebuild=True,
)


# Create Table
print("Create table")
t = Table("Comparison before and after Herbie", [
    ["Program", "l"],
    ["Before", "r"],
    ["After", "l"],
    ["Before", "r"],
    ["After", "l"],
])


t.add_top_header("Cancellation", 1, 2)
t.add_top_header("Absorption", 3, 2)

print(herbie_report_dict.__contains__("case_study"))

for name, after_r in herbie_report_dict.items():
    before_r = fpscan_report_dict[name]

    before_cancellation = len(before_r.get_cancellation())
    after_cancellation  = len(after_r.get_cancellation())

    before_absorption = len(before_r.get_absorption())
    after_absorption  = len(after_r.get_absorption())

    t.add_line([
        name,
        before_cancellation,
        after_cancellation,
        before_absorption,
        after_absorption
    ])

format_fun = lambda d: "\\textbf{" + d + "}"
format_color = lambda c: (lambda d: "{\\color{" + c + "} " + d + "}")
format_red = format_color("red")
format_green = format_color("green")


for line in t.lines:
    # cancellation
    b = line.get_val(1)
    a = line.get_val(2)
    if a < b:
        line.set_second_col_formatter(2, format_green)
    elif a > b:
        line.set_second_col_formatter(2, format_red)
    # absorption
    b = line.get_val(3)
    a = line.get_val(4)
    if a < b:
        line.set_second_col_formatter(4, format_green)
    elif a > b:
        line.set_second_col_formatter(4, format_red)

t.build_pdf(
    latex_dir=os.path.join("tex", "tex_herbie"),
    pdf_path="herbie.pdf"
)



