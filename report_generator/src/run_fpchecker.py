import os
import tempfile
import subprocess

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors

from .fpchecker.fpchecker import FPChecker
from .fpchecker.sample import Sample, RandomSample

def run_fpchecker(
    prgm_path:str,
    N_point: int,
    random_sample: bool=False,
    print_safe_point: bool = False,
    save: bool = False,
):
    prgm_path = os.path.abspath(prgm_path)
    if not random_sample:
        sample = Sample()
    else:
        sample = RandomSample()
    proj = None
    # if args.proj is None:
    #     proj=None
    # else:
    #     proj = list(map(lambda i: int(i) -1, args.proj))

    prgm_name = os.path.basename(prgm_path).replace(".fpcore", "")
    tmp_dir = tempfile.mkdtemp()

    fpc = FPChecker(
        prgm_name,
        prgm_path,
        tmp_dir
    )

    if proj is None:
        proj = list(range(len(fpc.variables)))

    if len(proj) != 2 and len(proj) != 3:
            raise Exception("Invalid projection" + str(proj))
        
    fpc.explore(
        sample,
        N_point
    )

    color_counter = 0
    all_colours = sorted(mcolors.XKCD_COLORS)
    line_plot_dict = {}
    def add_to_plot(color_counter: int, line: int, p):
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

    for entry in fpc.entries.values():
        for pitfall in entry:
            p = pitfall[0]
            line = pitfall[1]
            color_counter = add_to_plot(color_counter, line, p)

    for p in fpc.safe_points:
        add_to_plot(color_counter, 0, p)

    fig = plt.figure(figsize=[6, 6])
    if len(proj) == 3:
        ax = fig.add_subplot(projection='3d')
    else:
        ax = fig.add_subplot()

    ax.set_xlabel(fpc.variables[proj[0]], fontsize=20)
    ax.set_ylabel(fpc.variables[proj[1]], fontsize=20)
    if len(proj) == 3:
        ax.set_zlabel(fpc.variables[proj[2]], fontsize=20)
        # plt.zticks(fontsize=15)
        ax.tick_params('z', labelsize=15)
        ax.zaxis.labelpad = 15
    plt.xticks(fontsize=15, rotation=45)
    plt.yticks(fontsize=15)

    ax.xaxis.labelpad = 20
    ax.yaxis.labelpad = 15


    if print_safe_point:
        g = line_plot_dict[0]
        lp = g[0]
        c = g[1]

        label = "Safe points"
        if len(proj) == 2:
            ax.scatter(
                get_coord(proj[0], lp),
                get_coord(proj[1], lp),
                c=c, label=label)
        else:
            ax.scatter(
                get_coord(proj[0], lp),
                get_coord(proj[1], lp),
                get_coord(proj[2], lp),
                c=c, label=label)

    for line in line_plot_dict:
        g = line_plot_dict[line]
        lp = g[0]
        c = g[1]
        if line == 0:
            continue
        label = f"line: {fpc.line_to_code[line].strip()}"
        if len(proj) == 2:
            ax.scatter(
                get_coord(proj[0], lp),
                get_coord(proj[1], lp),
                c=c, label=label)
        else:
            ax.scatter(
                get_coord(proj[0], lp),
                get_coord(proj[1], lp),
                get_coord(proj[2], lp),
                c=c, label=label)
            
    if random_sample:
        ax.set_title("Random Sampling", fontsize=25)
    else:
        ax.set_title("Regular Sampling", fontsize=25)

    if len(line_plot_dict) > 1:
        if len(proj) == 2:
            box = ax.get_position()
            ax.set_position([box.x0, box.y0 + box.height * 0.1,
                            box.width, box.height * 0.9])

            # Put a legend below current axis
            ax.legend(loc='upper center', fontsize=15, bbox_to_anchor=(0.5, -0.15),
                    fancybox=True, shadow=True, ncol=2)
        else:
            box = ax.get_position()
            ax.set_position([box.x0, box.y0 + box.height * 0.1,
                            box.width, box.height * 0.9])

            # Put a legend below current axis
            ax.legend(loc='upper center', fontsize=15, bbox_to_anchor=(0.5, -0.15),
                    fancybox=True, shadow=True, ncol=2)

    if save:
        save_name = f"{prgm_name}_{'rand' if random_sample else 'reg'}_{N_point}"
        plt.savefig(f"{save_name}.svg")
        subprocess.run([
            "inkscape",
            f"{save_name}.svg",
            "-o", f"{save_name}.pdf",
            "--export-type", "pdf",
            "--export-area-drawing",
            "--vacuum-defs",
        ])
    else:
        plt.show()
