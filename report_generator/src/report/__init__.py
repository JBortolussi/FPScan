import yaml
import os
import pickle
from subprocess import TimeoutExpired

from .pitfall import Pitfall
from ..tiny import Tiny

class Report:
    def __init__(self, name = "None", time=None):
        self.name = name
        self.nb_op = 0
        self.nb_potential_pitfall = 0
        self.escaped_name = name.replace("_", "\_")
        self.pitfall_set = set({})
        self.sanity_check = False
        self.time = time

    def save(self, dir_path):
        file_path = os.path.join(dir_path, self.name + ".data")

        with open(file_path, "wb") as fd:
            pickle.dump(self, fd)

    def to_op_dict(self):
        op_pitfall_dict = {}

        for p in self.pitfall_set:
            pitfall_list = op_pitfall_dict.get(p.op, [])
            pitfall_list.append(p)
            op_pitfall_dict[p.op] = pitfall_list

        return op_pitfall_dict

    def save_yaml(self, path):
        D = {
            "time": self.time,
            "sanity_check": self.sanity_check,
            "report": []
        }

        op_pitfall_dict = {}

        for p in self.pitfall_set:
            pitfall_list = op_pitfall_dict.get(p.op, [])
            pitfall_list.append(p)
            op_pitfall_dict[p.op] = pitfall_list

        for op in op_pitfall_dict:
            pitfall_list = op_pitfall_dict[op]
            loc = pitfall_list[0].loc
            D["report"].append(
                [
                    loc,
                    op,
                    [
                        p.id for p in pitfall_list
                    ]
                ]
            )
            

        with open(path, "w") as fd:
            yaml.dump(D, fd)

    def add_report_entry(self, line):
        loc = line[0]
        op = line[1]
        if ("+" in op) or ("-" in op):
            self.nb_potential_pitfall += 3
        self.nb_op += 1

        if line[2] == []:
            self.pitfall_set.add(Pitfall("None", loc, op))
        else:
            for p in line[2]:
                new_pitfall = Pitfall(p, loc, op)
                self.pitfall_set.add(new_pitfall)

    @staticmethod
    def load_yaml(name, report_path):
        with open(report_path) as fd:
            D = yaml.safe_load(fd)

        r = Report(name=name, time=D["time"])

        r.sanity_check = D["sanity_check"]

        for line in D["report"]:
           r.add_report_entry(line)
        return r
        
    @staticmethod
    def build(name, prgm_path, report_dir_path, run_tiny=None, force_rebuild=False):
        cached_report_path = os.path.join(report_dir_path, name + "_report.yaml")
        report_path = os.path.join(report_dir_path, name + ".yaml")

        # need to rebuild ?
        if (force_rebuild or not os.path.exists(cached_report_path)):
            # print("rebuild")
            if run_tiny is None:
                run_tiny = Tiny("./tiny")
            try:
                run_result = run_tiny.run(prgm_path, report_path)
                if run_result is None:
                    raise Exception("build failed")
                report_path, duration = run_result
                r = Report.load_from_tiny(name, report_path)
                r.set_time(duration)
            except TimeoutExpired:
                r = Report(name)
            r.save_yaml(cached_report_path)
        else:
            r = Report.load_yaml(name, cached_report_path)

        return r

    def load_from_tiny(name, file):
        # name = os.path.split(file)[-1].split(".")[0]
        report=Report(name)
        with open(file) as f:
            try:
                raw_report = yaml.safe_load(f)
            except Exception as e:
                print("Impossible to read yaml report")
                exit

            report.sanity_check = raw_report["sanity_check"]
                
            for entry in raw_report["report"]:
                report.add_report_entry(entry)
        
        return report
    
    def load_save(save_path):
        with open(save_path, "rb") as fd:
            return pickle.load(fd)
    
    def set_time(self, time):
        self.time = time

    def get_absorption(self):
        return {p for p in self.pitfall_set if "absorption" in p.id}
    

    def get_cancellation(self):
        return {p for p in self.pitfall_set if "cancellation" in p.id}
    

    def contains_pitfall(self, p):
        for pitfall in self.pitfall_set:
            if p == pitfall:
                return True
        return False