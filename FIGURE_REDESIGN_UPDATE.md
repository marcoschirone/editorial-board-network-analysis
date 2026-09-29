# Figure architecture update — V5.5

This version adopts a conservative Round-2 publication strategy.

- **Figure 1 — Geographic distribution of editors.** Retain the reviewer-accepted static map and interactive supplementary map from the separate map workflow. The main `targets` pipeline does not regenerate or replace Figure 1.
- **Figure 2 — Interlocking-editor network across analytical attributes.** The full 80-editor network is shown with identical coordinates in three panels: (A) weighted EVC in Viridis, recalculated on the full graph for descriptive visualization with degree by node size; (B) completed Female/Male gender classification; and (C) continent of primary institutional affiliation. Panels B and C are descriptive only. Primary prominence inference remains based on the 78-node giant component.
- **Figure 3 — Composition of reference populations and interlocking editors.** Panel A compares all 2,033 editors with the 80 interlocking editors by continent. Panel B compares all 1,797 confidently NamSor-classified editors with the 69 confidently classified interlocking editors; the remaining 11 of the 80 interlocking editors are below the .70 NamSor confidence threshold and are excluded from this panel by gender. These panels are descriptive; inferential tests remain in the manuscript text.
- **Figure 4 — Journal community structure.** The nine-community Leiden solution is unchanged; singleton communities share a neutral display treatment only.
- **Figure 5 — Journal-level prominence and inequality.** Median member EVC and within-board raw Gini use a fixed common journal layout.

The former subregional main-text figure, gender-coded editor network, and proportional-symbol replacement for Figure 1 are retired. The disparity dashboard remains supplementary/exploratory.

Regenerate repository-managed manuscript figures with:

```bash
Rscript -e 'targets::tar_make(c(figure_2_plot, figure_3_plot, figure_4_plot, figure_5_plot))'
```

## V5.10 final Figure 2 production refinement

Figure 2 retains the horizontal A-B-C architecture and the identical fixed 80-editor layout in all panels. The Panel A EVC and degree legends are now horizontal and placed below the network, matching the legend position used in Panels B and C. This removes the previous right-side legend column and gives all three panels equal plotting width. No analytical quantities, node coordinates, classifications, or inferential procedures were changed.
