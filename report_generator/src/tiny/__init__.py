import time
import subprocess

class Tiny:
    def __init__(self, tiny_path, timeout=None, opt=[]):
        self.opt = opt
        self.tiny_path=tiny_path
        self.timeout = timeout

    def set_opt(self, opt):
        self.opt = opt

    def run(self, prgm_path, report_path):
        cmd = [self.tiny_path, prgm_path, "-yaml", report_path]
        cmd.extend(self.opt)

        start_time = time.time_ns()
        f = open("log.txt", "w")
        result = subprocess.run(cmd, capture_output=True, timeout=self.timeout)
        f.close()
        duration = (time.time_ns() - start_time) * 10**-9

        if result.returncode != 0:  
            # print("Analizis failed")
            print(result.stdout.decode("utf-8"))
            print(result.stderr.decode("utf-8"))
            raise Exception(f"Analizis failed: {prgm_path}")
        else:
            # r = Report.load(report_path)
            # r.set_time(duration)
            return report_path, duration