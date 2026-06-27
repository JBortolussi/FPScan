import os
from .tiny import Tiny
import tempfile

import yaml

fpcore_dir = "fpcore_prgm"

trigo_fun = [
    "sin",
    "cos",
    "atan",
    # "sqrt"
]

# known_faillures = set({
#     "hartman6.fpcore",
# 	"triangle5.fpcore",
# 	"NMSE_example_3.4.fpcore",
# 	"NMSE_example_3.8.fpcore",
# 	"triangle2.fpcore",
# 	"Complex_sine_and_cosine.fpcore",
# 	"triangle6.fpcore",
# 	"Jacobi_s_Method.fpcore",
# 	"smartRoot.fpcore",
# 	"triangle8.fpcore",
# 	"i4.fpcore",
# 	"triangle9.fpcore",
# 	"NMSE_problem_3.4.2.fpcore",
# 	"arclength_of_a_wiggly_function__old_version_.fpcore",
# 	"Eigenvalue_Computation.fpcore",
# 	"Iterative_Gram_Schmidt_Method.fpcore",
# 	"NMSE_section_3.5.fpcore",
# 	"NMSE_p42__positive.fpcore",
# 	"Trapeze.fpcore",
# 	"carthesianToPolar__theta.fpcore",
# 	"instantaneousCurrent.fpcore",
# 	"NMSE_section_3.11.fpcore",
# 	"NMSE_problem_3.4.3.fpcore",
# 	"NMSE_problem_3.3.7.fpcore",
# 	"floudas.fpcore",
# 	"NMSE_problem_3.4.5.fpcore",
# 	"triangle4.fpcore",
# 	"NMSE_example_3.7.fpcore",
# 	"Probabilities_in_a_clustering_algorithm.fpcore",
# 	"NMSE_problem_3.4.6.fpcore",
# 	"exp1x_32.fpcore",
# 	"triangleSorted.fpcore",
# 	"PID.fpcore",
# 	"Odometry.fpcore",
# 	"N_Body_Simulation.fpcore",
# 	"triangle12.fpcore",
# 	"exp1x.fpcore",
# 	"floudas3.fpcore",
# 	"exp1x_log.fpcore",
# 	"Lead_lag_System.fpcore",
# 	"NMSE_problem_3.3.1.fpcore",
# 	"NMSE_example_3.6.fpcore",
# 	"NMSE_problem_3.3.2.fpcore",
# 	"floudas1.fpcore",
# 	"triangle3.fpcore",
# 	"Newton_Raphson_s_Method.fpcore",
# 	"NMSE_problem_3.3.3.fpcore",
# 	"floudas2.fpcore",
# 	"NMSE_problem_3.4.1.fpcore",
# 	"Gustafson_s_example.fpcore",
# 	"NMSE_example_3.10.fpcore",
# 	"triangle7.fpcore",
# 	"carthesianToPolar__radius.fpcore",
# 	"triangle10.fpcore",
# 	"Pendulum.fpcore",
# 	"Runge_Kutta_4.fpcore",
# 	"triangle1.fpcore",
# 	"NMSE_p42__negative.fpcore",
# 	"NMSE_problem_3.2.1__positive.fpcore",
# 	"Sine_Newton.fpcore",
# 	"NMSE_example_3.1.fpcore",
# 	"NMSE_problem_3.3.6.fpcore",
# 	"arclength_of_a_wiggly_function.fpcore",
# 	"Rocket_Trajectory.fpcore",
# 	"NMSE_example_3.9.fpcore",
# 	"NMSE_problem_3.4.4.fpcore",
# 	"triangle11.fpcore",
# 	"hartman3.fpcore",
# 	"Complex_square_root.fpcore",
# 	"NMSE_problem_3.3.4.fpcore",
# 	"logexp.fpcore",
# 	"NMSE_problem_3.2.1__negative.fpcore",
# })

known_faillures_no_pre = {
    "Rump_s_example_revisited_for_floating_point.fpcore", # no pre
    "NMSE_problem_3.3.5.fpcore", # no pre
    "Rump_s_example__with_pow.fpcore", # no pre
    "Rump_s_example__from_C_program.fpcore", # no pre
    "NMSE_example_3.3.fpcore", # no pre
    "NMSE_example_3.5.fpcore", # no pre
    "Shoelace_formula.fpcore", # no pre
}

exclude_prgm = {
    "NMSE_problem_3.3.1.fpcore",
    "NMSE_example_3.4.fpcore",
    "jetEngine.fpcore",
}

def known_failures():
    if os.path.exists("known_failures.yaml"):
        with open("known_failures.yaml", "r") as fd:
            return set(yaml.safe_load(fd)).union(known_faillures_no_pre).union(exclude_prgm)
    else:
        return save_known_failures().union(known_faillures_no_pre).union(exclude_prgm)

def save_known_failures():
    prgm = check()
    with open("known_failures.yaml", "w") as fd:
        yaml.dump(prgm, fd)
    return set(prgm)

def mk_fpcore_path(name):
    return os.path.join(fpcore_dir, name)

def get_fpcore_prgm(skip_known_faillures=True) -> tuple[set[str], set[str]]:
    prgm_set = set(os.listdir(fpcore_dir))  
    if skip_known_faillures:
        prgm_set = prgm_set - known_failures()
    trigo_set: set[str] = set({})
    for prgm in prgm_set:
        with open(mk_fpcore_path(prgm)) as fd:
            code = fd.read()
            for f in trigo_fun:
                if f in code:
                    trigo_set.add(prgm)
                    break
    
    return prgm_set, trigo_set

def get_fpcore_prgm_path():
    return {
        mk_fpcore_path(name) for name in get_fpcore_prgm()
    }

def check() -> list[str]:
    run_tiny = Tiny("./fpscan.exe", opt=["-fpcore"])

    prgm_list, trigo_prgm = get_fpcore_prgm(skip_known_faillures=False)

    report_dir = tempfile.TemporaryDirectory()
    report_dir_path = report_dir.name

    prgm_failling = []
    prgm_passing = []

    print(f"tot: {len(prgm_list)}")
    index = 0

    for prgm_name in prgm_list:
        prgm_path = mk_fpcore_path(prgm_name)
        index += 1
        print(f"{index}: {prgm_path}")

        report_name = prgm_name.replace(".fpcore", ".yaml")
        report_path = os.path.join(report_dir_path, report_name)

        try:
            r = run_tiny.run(prgm_path, report_path)
            prgm_passing.append(prgm_name)
        except Exception as e:
            prgm_failling.append(prgm_name)

    report_dir.cleanup()

    print(f"{len(prgm_passing)} are running properly and {len(prgm_failling)} are not.")
    if prgm_failling:
        print(f"{len(prgm_failling)} new failure detected")
        for prgm in prgm_failling:
            print(f"\t\"{prgm}\",")
            
    return prgm_failling