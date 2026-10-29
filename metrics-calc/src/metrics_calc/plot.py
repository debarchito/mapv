from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from matplotlib.lines import Line2D
from matplotlib.patches import Patch

MODES = ("off", "on")
MODE_LABELS = {"off": "Off", "on": "On"}
MODE_NAMES = {"off": "Tracing off", "on": "Tracing on"}
MODE_COLORS = {"off": "#4C72B0", "on": "#DD8452"}
GC_COLORS = {"minor": "#55A868", "major": "#C44E52"}
GC_NAMES = {"minor": "Minor GC", "major": "Major GC"}
PLOT_NAMES = {"no_gc": "no_gc_pressure"}
DISPLAY_COLUMNS = (
    "phase",
    "workload",
    "tracing",
    "exec_wall_ns",
    "rss_peak_delta_kb",
    "mapv_gc_minor_ns",
    "mapv_gc_major_ns",
    "mapv_gc_minor_count",
    "mapv_gc_major_count",
)


def _load_runs(metrics_dir: Path) -> pd.DataFrame:
    path = metrics_dir / "metrics_runs.csv"
    if not path.is_file():
        raise FileNotFoundError(f"missing metrics dump: {path}")

    runs = pd.read_csv(path)
    missing = set(DISPLAY_COLUMNS) - set(runs.columns)
    if missing:
        raise ValueError(f"{path} is missing columns: {', '.join(sorted(missing))}")

    runs = runs[(runs["phase"] == "measure") & runs["tracing"].isin(MODES)].copy()
    if runs.empty:
        raise ValueError(f"{path} has no measured off/on rows")

    for column in DISPLAY_COLUMNS:
        if column not in {"phase", "workload", "tracing"}:
            runs[column] = pd.to_numeric(runs[column], errors="raise")

    runs["exec_ms"] = runs["exec_wall_ns"] / 1e6
    runs["rss_mib"] = runs["rss_peak_delta_kb"] / 1024
    runs["gc_minor_ms"] = runs["mapv_gc_minor_ns"] / 1e6
    runs["gc_major_ms"] = runs["mapv_gc_major_ns"] / 1e6
    runs["gc_total_ms"] = runs["gc_minor_ms"] + runs["gc_major_ms"]
    return runs


def _stats(values: pd.Series) -> tuple[float, float, float]:
    array = values.to_numpy(dtype=float)
    return (
        float(np.median(array)),
        float(np.quantile(array, 0.25)),
        float(np.quantile(array, 0.75)),
    )


def _draw_bar_panel(
    ax: plt.Axes,
    workload_runs: pd.DataFrame,
    column: str,
    *,
    title: str,
    ylabel: str,
) -> float:
    medians: dict[str, float] = {}
    maxima: list[float] = []
    for index, mode in enumerate(MODES):
        values = workload_runs.loc[workload_runs["tracing"] == mode, column]
        median, q1, q3 = _stats(values)
        medians[mode] = median
        maxima.append(float(values.max()))

        ax.bar(
            index,
            median,
            width=0.56,
            color=MODE_COLORS[mode],
            edgecolor="#222222",
            linewidth=0.9,
            zorder=2,
        )
        ax.errorbar(
            index,
            median,
            yerr=[[median - q1], [q3 - median]],
            fmt="none",
            ecolor="#222222",
            elinewidth=1.2,
            capsize=7,
            zorder=4,
        )

        jitter = (
            np.linspace(-0.12, 0.12, len(values))
            if len(values) > 1
            else np.zeros(len(values))
        )
        ax.scatter(
            index + jitter,
            values,
            s=42,
            facecolor="white",
            edgecolor="#222222",
            linewidth=1.0,
            zorder=5,
        )
        ax.annotate(
            f"{median:,.2f}",
            (index, median),
            xytext=(0, 11),
            textcoords="offset points",
            ha="center",
            va="bottom",
            fontsize=13,
            fontweight="bold",
        )

    ax.set_xticks(range(len(MODES)), [MODE_LABELS[mode] for mode in MODES])
    ax.set_xlim(-0.6, 1.6)
    ax.set_ylim(0, max(maxima) * 1.25)
    ax.set_ylabel(ylabel)

    ratio = medians["on"] / medians["off"]
    ax.set_title(
        f"{title} ({ratio:.1f}×)",
        loc="left",
        fontsize=16,
        fontweight="bold",
        pad=14,
    )
    return ratio


def _draw_gc_panel(ax: plt.Axes, workload_runs: pd.DataFrame) -> float:
    totals: dict[str, float] = {}
    minor_counts: dict[str, int] = {}
    major_counts: dict[str, int] = {}
    all_totals: list[float] = []

    for index, mode in enumerate(MODES):
        mode_runs = workload_runs.loc[workload_runs["tracing"] == mode]
        minor, _, _ = _stats(mode_runs["gc_minor_ms"])
        major, _, _ = _stats(mode_runs["gc_major_ms"])
        total, q1, q3 = _stats(mode_runs["gc_total_ms"])
        minor_count = round(_stats(mode_runs["mapv_gc_minor_count"])[0])
        major_count = round(_stats(mode_runs["mapv_gc_major_count"])[0])

        totals[mode] = total
        minor_counts[mode] = minor_count
        major_counts[mode] = major_count
        all_totals.extend(mode_runs["gc_total_ms"].tolist())

        ax.bar(
            index,
            minor,
            width=0.56,
            color=GC_COLORS["minor"],
            edgecolor="#222222",
            linewidth=0.9,
            zorder=2,
        )
        ax.bar(
            index,
            major,
            bottom=minor,
            width=0.56,
            color=GC_COLORS["major"],
            edgecolor="#222222",
            linewidth=0.9,
            zorder=3,
        )
        ax.errorbar(
            index,
            total,
            yerr=[[total - q1], [q3 - total]],
            fmt="none",
            ecolor="#222222",
            elinewidth=1.2,
            capsize=7,
            zorder=4,
        )

        values = mode_runs["gc_total_ms"]
        jitter = (
            np.linspace(-0.12, 0.12, len(values))
            if len(values) > 1
            else np.zeros(len(values))
        )
        ax.scatter(
            index + jitter,
            values,
            s=42,
            facecolor="white",
            edgecolor="#222222",
            linewidth=1.0,
            zorder=5,
        )
        ax.annotate(
            f"{total:,.2f}",
            (index, total),
            xytext=(0, 11),
            textcoords="offset points",
            ha="center",
            va="bottom",
            fontsize=13,
            fontweight="bold",
        )

    tick_labels = [
        f"{MODE_LABELS[mode]}\n{minor_counts[mode]} minor, {major_counts[mode]} major"
        for mode in MODES
    ]
    ax.set_xticks(range(len(MODES)), tick_labels)
    ax.set_xlim(-0.6, 1.6)
    ax.set_ylim(0, max(all_totals) * 1.25)
    ax.set_ylabel("GC wall time (ms)")

    ratio = totals["on"] / totals["off"]
    ax.set_title(
        f"(c) GC time ({ratio:.1f}×)",
        loc="left",
        fontsize=16,
        fontweight="bold",
        pad=14,
    )
    return ratio


def _plot_name(workload: str) -> str:
    return PLOT_NAMES.get(workload, workload)


def _plot_workload(
    workload: str,
    workload_runs: pd.DataFrame,
    out_dir: Path,
) -> tuple[Path, Path]:
    sns.set_theme(
        style="whitegrid",
        context="talk",
        rc={
            "font.family": "serif",
            "font.serif": ["STIXGeneral", "Times New Roman", "DejaVu Serif"],
            "mathtext.fontset": "stix",
            "axes.edgecolor": "#333333",
            "axes.linewidth": 0.8,
            "grid.color": "#DDDDDD",
            "grid.linewidth": 0.7,
        },
    )

    figure = plt.figure(figsize=(14, 5.8), facecolor="white")
    has_gc_panel = workload == "gc_pressure"
    ncols = 3 if has_gc_panel else 2
    grid = figure.add_gridspec(
        1,
        ncols,
        left=0.065 if has_gc_panel else 0.075,
        right=0.985,
        top=0.855,
        bottom=0.17 if has_gc_panel else 0.13,
        wspace=0.28 if has_gc_panel else 0.22,
    )
    exec_ax = figure.add_subplot(grid[0, 0])
    memory_ax = figure.add_subplot(grid[0, 1])
    axes = [exec_ax, memory_ax]

    _draw_bar_panel(
        exec_ax,
        workload_runs,
        "exec_ms",
        title="(a) Execution time",
        ylabel="Execution wall time (ms)",
    )
    _draw_bar_panel(
        memory_ax,
        workload_runs,
        "rss_mib",
        title="(b) Peak memory usage",
        ylabel="Peak RSS growth (MiB)",
    )
    if has_gc_panel:
        gc_ax = figure.add_subplot(grid[0, 2])
        _draw_gc_panel(gc_ax, workload_runs)
        axes.append(gc_ax)

    for axis in axes:
        axis.set_axisbelow(True)
        axis.grid(axis="y", color="#DDDDDD", linewidth=0.7)
        axis.grid(axis="x", visible=False)
        axis.tick_params(axis="both", which="major", labelsize=12, width=0.8, length=4)
        sns.despine(ax=axis, left=False, bottom=False)

    legend_handles = [
        Patch(
            facecolor=MODE_COLORS["off"],
            edgecolor="#222222",
            linewidth=0.9,
            label=MODE_NAMES["off"],
        ),
        Patch(
            facecolor=MODE_COLORS["on"],
            edgecolor="#222222",
            linewidth=0.9,
            label=MODE_NAMES["on"],
        ),
        Line2D(
            [],
            [],
            marker="o",
            linestyle="none",
            markerfacecolor="white",
            markeredgecolor="#222222",
            markeredgewidth=1.0,
            markersize=6,
            label="Individual run",
        ),
    ]
    if has_gc_panel:
        legend_handles.extend(
            [
                Patch(
                    facecolor=GC_COLORS["minor"],
                    edgecolor="#222222",
                    linewidth=0.9,
                    label=GC_NAMES["minor"],
                ),
                Patch(
                    facecolor=GC_COLORS["major"],
                    edgecolor="#222222",
                    linewidth=0.9,
                    label=GC_NAMES["major"],
                ),
            ]
        )

    figure.legend(
        handles=legend_handles,
        loc="upper center",
        bbox_to_anchor=(0.5, 0.985),
        ncol=5 if has_gc_panel else 3,
        frameon=False,
        fontsize=11,
        handlelength=1.2,
        handletextpad=0.5,
        columnspacing=1.6,
    )

    out_dir.mkdir(parents=True, exist_ok=True)
    name = _plot_name(workload)
    png_path = out_dir / f"{name}.png"
    pdf_path = out_dir / f"{name}.pdf"
    figure.savefig(png_path, dpi=300, bbox_inches="tight", facecolor="white")
    figure.savefig(pdf_path, bbox_inches="tight", facecolor="white")
    plt.close(figure)
    return png_path, pdf_path


def main() -> None:
    parser = argparse.ArgumentParser(
        description=(
            "Plot execution, memory, and Mapv GC overhead from metrics_runs.csv"
        )
    )
    parser.add_argument(
        "--metrics-dir",
        type=Path,
        default=Path("metrics"),
        help="directory containing metrics_runs.csv (default: metrics)",
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path("metrics/plots"),
        help="directory for generated plots (default: metrics/plots)",
    )
    args = parser.parse_args()

    runs = _load_runs(args.metrics_dir)
    for workload, workload_runs in runs.groupby("workload", sort=False):
        paths = _plot_workload(str(workload), workload_runs, args.out_dir)
        print(f"{_plot_name(str(workload))}: {paths[0]} + {paths[1]}")


if __name__ == "__main__":
    main()
