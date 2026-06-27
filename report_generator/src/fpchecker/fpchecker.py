import os
import subprocess
import shutil
import json
import hashlib
import time

from ..report import Report

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors


FPCORE_TO_C = "./fpcore_to_C.exe"
FPCHECKER_LOG = ".fpc_logs"

class FPChecker:
    class Pts:
        def __init__(self, pts):
            self.pts = pts
            self.id = "_".join([str(p) for p in self.pts])

        def __hash__(self):
            return int(hashlib.sha1(self.id.encode()).hexdigest(), 16)
        
        def __eq__(self, value):
            return self.__hash__() == value.__hash__()

    def __init__(self, prgm_name: str, prgm_path: str, dir_path: str) -> None:
        self.name = prgm_name
        self.fpcore_prgm_path = os.path.abspath(prgm_path)
        self.dir = os.path.abspath(dir_path)
        self.C_prgm_path = os.path.join(self.dir, self.name + ".c")
        self.exe_path = os.path.join(self.dir, "prgm")
        # Line 0 dosn(t exist)
        self.line_to_code = ["no line 0"]

        self.pts: list[list[float]] = []
        self.entries = {}
        self.safe_points: list[list[float]] = []
        self.time: float = 0
        self.variables: list[str] = []
        self.ranges: list[tuple[float, float]] = []

        if not os.path.exists(self.dir):
            os.mkdir(self.dir)

        # compile to C
        r = subprocess.run([FPCORE_TO_C, "-f", self.fpcore_prgm_path, "-o", self.C_prgm_path], capture_output=True)
        if r.returncode != 0:
            err = r.stderr.decode("utf-8")
            raise Exception(
                f"impossible to produce C code for {prgm_name}: {err}"
            )

        # compile
        cmd = [
            "clang",
            "-g",
            "-include",
            "FPChecker_install/src/Runtime_cpu.h",
            "-fpass-plugin=FPChecker_install/lib/libfpchecker_cpu.so",
            "-lm",
            "-o",
            self.exe_path,
            self.C_prgm_path
        ]
        r = subprocess.run(cmd, capture_output=True)
        if r.returncode != 0:
            err = r.stderr.decode("utf-8")
            raise Exception(
                f"impossible to compile C code for {prgm_name}: {err}"
            )
        
        # Extract lines
        with open(self.C_prgm_path) as fd:
            l = fd.readlines()
            for i, line in enumerate(l):
                self.line_to_code.append(line)

        # Extract ranges
        with open(self.C_prgm_path) as fd:
            l = fd.readlines()
            for line in l:
                if "//" in line:
                    line = line.replace("//", "").split(":")
                    v = line[0]
                    line = line[1].strip()
                    line = line.split(";")
                    l = float(line[0].replace("[", ""))
                    u = float(line[1].replace("]", ""))
                    self.ranges.append([l, u])
                    self.variables.append(v)

    def clean_reports(self):
        report_dir = os.path.join(self.dir, FPCHECKER_LOG)
        if os.path.exists(report_dir):
            shutil.rmtree(report_dir)


    def call(self, arguments):
        old_dir = os.getcwd()
        os.chdir(self.dir)
        cmd = [self.exe_path]
        cmd.extend([str(arg) for arg in arguments])
        subprocess.run(cmd, capture_output=True)
        os.chdir(old_dir)

    def load_json(self, path):
        path = os.path.join(FPCHECKER_LOG, path)
        with open(path) as fd:
            t = json.load(fd)

        pitfalls = []

        input_key = None

        for entry in t:
            if entry["cancellation"] == "0":
                continue

            inputs = entry["input"].strip().split(" ")
            inputs = [float(i) for i in inputs[1:]]
            pitfalls.append([
                inputs,
                int(entry["line"])
            ])
            input_key = FPChecker.Pts(inputs)

        return input_key, pitfalls
    
    def explore(self, sample, N):
        cwd = os.getcwd()
        os.chdir(self.dir)
        if os.path.exists(FPCHECKER_LOG):
            shutil.rmtree(FPCHECKER_LOG)

        start_time = time.time_ns()
        # sample
        self.pts = sample.sample(N, self.ranges)

        # analyse
        for p in self.pts:
            self.call(p)

        # collect results
        self.entries = {}
        for f_path in os.listdir(FPCHECKER_LOG):
            key, entry = self.load_json(f_path)
            if key is not None:
                self.entries[key] = entry
        
        for p in self.pts:
            if FPChecker.Pts(p) not in self.entries:
                self.safe_points.append(p)

        self.time = (time.time_ns() - start_time) * 10**-9
        os.chdir(cwd)

    def plot_2D(self):
        color_counter = 0
        all_colours = sorted(mcolors.XKCD_COLORS)
        line_plot_dict = {}
        def add_to_plot(color_counter, line, p):
            if line in line_plot_dict:
                line_plot_dict[line][0].append(p)
            else:
                color_counter += 1
                c = all_colours[color_counter]
                line_plot_dict[line] = [[p], c]
            return color_counter

        def get_coord(i, l):
            X = []
            for p in l:
                X.append(p[i])
            return X

        for p in self.pts:
            for entry in self.entries.get(FPChecker.Pts(p), []):
                line = entry[1]
                color_counter = add_to_plot(color_counter, line, p)
                

        fig = plt.figure()
        ax = fig.add_subplot(projection='3d')
        ax.set_xlabel('X')
        ax.set_ylabel('Y')
        ax.set_zlabel('Z')

        for line in line_plot_dict:
            g = line_plot_dict[line]
            lp = g[0]
            c = g[1]
            X = get_coord(0, lp)
            Y = get_coord(1, lp)
            Z = get_coord(2, lp)
            ax.scatter(X, Y, Z, c=c, label=f"{line}: {c}")
        
        fig.legend()
        plt.show()

    def mk_report(self):
        r = Report(self.name, time=self.time)


        line_op_dict = {}
        # add to dict all pitfalls
        for e in self.entries:
            line = self.entries[e][0][1]
            if line not in line_op_dict:
                line_op_dict[line] = self.line_to_code[line].strip().replace(";", "").strip()
            
        for op in line_op_dict.values():
            r.add_report_entry(["", op, ["cancellation"]])
            
        return r