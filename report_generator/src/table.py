import os
import subprocess
import shutil
import matplotlib.pyplot as plt

BOT = "$\\bot$"

def bf(text):
    return f"\\textbf({text})"

class Line:
    def __init__(self, data, formatter=None):
        self.data = data
        self.line_formatter = formatter
        self.data_formatter = [None for _ in data]
        self.second_fomatter = [None for _ in data]

    def set_second_col_formatter(self, col, fmt):
        self.second_fomatter[col] = fmt

    def len(self):
        return len(self.data)

    def get_val(self, i):
        return self.data[i]
    
    def get_str(self, i, formatter = None):
        if self.data_formatter[i] is not None:
            formatter = self.data_formatter[i]
        elif self.line_formatter is not None:
            formatter = self.line_formatter
        elif formatter is None:
            formatter = lambda d: str(d)

        if self.second_fomatter[i] is None:
            snd_formatter = lambda d: d
        else:
            snd_formatter = self.second_fomatter[i]

        return snd_formatter(formatter(self.get_val(i)))

    

class Table:
    def __init__(self, name, cols):
        self.cols = []
        self.col_header = []
        for col in cols:
            header = "c"
            if isinstance(col, list):
                header = col[1]
                col = col[0]
            self.col_header.append(header)
            self.cols.append([col, 0])
        # self.cols = [[col, 0] for col in cols]
        self.lines = []
        self.name = name

        self.col_formatter = [None for _ in cols]
        # self.col_header = ["r" for _ in cols]
        self.col_header[0] = "l"

        # [[header_txt, col_in, col_nb]]
        self.top_header = []


    def add_line(self, line, formatter=None):
        if len(line) != len(self.cols):
            raise Exception(f"bab number of cols: {len(line)} <> {len(self.cols)}")
        self.lines.append(Line(line, formatter))

    def sort(self, key):
        self.lines = sorted(self.lines, key=key, )

    def set_col_formatter(self, i, formatter):
        self.col_formatter[i] = formatter

    def set_col_header(self, i, header):
        self.col_header[i] = header

    def add_top_header(self, label, col_in, nb):
        self.top_header.append([label, col_in, nb])

    def _recompute_col_lenght(self):
        self.cols = [[col[0], 0] for col in self.cols]

        for line in self.lines:
            for i in range(line.len()):
                s = line.get_str(i, self.col_formatter[i])
                self.cols[i][1] = max(self.cols[i][1], len(s))

    def format_line(self, line):
        item_str_list = []
        for i, col in enumerate(self.cols):
            item = line.get_str(i, self.col_formatter[i])
            item = item.replace("_", "\_")
            item_str_list.append(f"{item: <{col[1]}}")
        return " & ".join(item_str_list)

    def to_latex_table(self):
        self._recompute_col_lenght()

        # make top_header
        top_header = []
        bot_header = []
        k = 0
        top_only = True
        while k < len(self.cols):
            # check if a top_header exist
            hd = None
            for decl_hd in self.top_header:
                if decl_hd[1] == k:
                    hd = "\\multicolumn{" + str(decl_hd[2]) + "}{c}{" + decl_hd[0] + "}"
                    for l in range (decl_hd[2]):
                        bot_header.append(self.cols[l + k][0])
                    k += decl_hd[2]
                    top_only = False
                    break
            if hd is None:
                hd = self.cols[k][0]
                bot_header.append("")
                k += 1
            top_header.append(hd)

        top_header = " & ".join(top_header)
        bot_header = " & ".join(bot_header)

        if top_only:
            header = top_header + "\\\\\n\\midrule\n"
        else:
            header = top_header + "\\\\\n\\midrule\n" + bot_header + "\\\\\n"


        latex_header = " & ".join([
            col[0] for col in self.cols
        ]) + "\\\\\n"

        # col_fmt_header = "\\begin{tabular}{@{}l" + ("r" * (len(self.cols) - 1)) + "@{}}\n"
        col_fmt_header = "\\begin{tabular}{@{}" + "".join(self.col_header) + "@{}}\n"

        table_body = "\\\\\n".join(
            self.format_line(line) for line in self.lines
        ) + "\\\\"

        return "".join([
            "\\begin{table*}[h]\n",
            "\\caption{" + self.name + "}\\vspace{0.1cm}\n",
            "\\label{tab::results}\n",
            "\\centering\n",
            "\\scriptsize\n",
            "\\renewcommand{\\arraystretch}{1.2}\n",
            "\\setlength{\\tabcolsep}{5pt}\n",
            col_fmt_header,
            # "\\toprule\n",
            # latex_header,
            # "\\midrule\n",
            header,
            table_body,
            "\n",
            "\\bottomrule\n",
            "\\end{tabular}\n",
            "\\end{table*}\n",
        ])
    
    def build_pdf(self, latex_dir="tex", pdf_path=None):
        pdf_path = os.path.relpath(f"{self.name}.pdf") if pdf_path is None else pdf_path
        if not os.path.exists(latex_dir):
            os.makedirs(latex_dir)

        print(pdf_path)
        print(latex_dir)

        main_file = os.path.join(latex_dir, "main.tex")
        table_file = os.path.join(latex_dir, "table.tex")
        document_body = "\n".join([
            "\\documentclass[a4paper]{article}",
            "\\usepackage{booktabs}",
            "\\usepackage{array}",
            "\\usepackage[table]{xcolor}",
            "",
            "\\begin{document}",
            "\\input{table}",
            "\\end{document}",
        ])

        with open(main_file, "w") as fd:
            fd.write(document_body)

        with open(table_file, "w") as fd:
            fd.write(self.to_latex_table())

        result = subprocess.run(["pdflatex", "main.tex"], cwd=latex_dir, stdout=subprocess.PIPE, stdin=subprocess.PIPE)
        if result.returncode != 0:
            print(result.stdout.decode('utf-8'))
            # print(result.stderr.decode('utf-8'))
        else:
            shutil.copyfile(os.path.join(latex_dir, f"main.pdf"), pdf_path)

    def to_csv(self, csv_path):
        op_t_map = {}
        for l in self.lines:
            n = int(l[2])

            old_t, old_k = op_t_map.get(n, (0, 0))
            t = old_t + float(l[1])
            k = old_k + 1
            op_t_map[n] = (t, k)

        collapsed_map = {
            n:t/k for n, (t, k) in op_t_map.items()
        }
            
        X = list(collapsed_map.keys())
        X.sort()
        print(X)
        Y = [collapsed_map[x] / x for x in X]

        print(X)
        print(Y)

        plt.plot(X, Y)
        plt.show()




