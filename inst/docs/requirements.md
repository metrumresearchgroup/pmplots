
Requirements for pharamcometric plotting package `pmplots`
==========================================================

<table>
<colgroup>
<col width="35%" />
<col width="7%" />
<col width="56%" />
</colgroup>
<thead>
<tr class="header">
<th>Section header</th>
<th>RID</th>
<th>Requirement</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><p><strong>Default column names</strong></p></td>
<td><p>1</p></td>
<td><p>Column <code>RES</code> refers to residual; rendered <code>res</code> in function names</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>2</p></td>
<td><p>Column <code>WRES</code> weighted residual; rendered <code>wres</code> in function names</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>3</p></td>
<td><p>Column <code>CWRES</code> refers to conditional weighted residual; rendered <code>cwres</code> in function names</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>4</p></td>
<td><p>Column <code>TIME</code> refers to model time; rendered <code>time</code> in function names</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>5</p></td>
<td><p>Column <code>TAFD</code> refers to time after first dose; rendered <code>tafd</code> in function names</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>6</p></td>
<td><p>Column <code>TAD</code> refers to time after dose; rendered <code>tad</code> in function names</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>7</p></td>
<td><p>Column <code>DV</code> refers to observed data; rendered <code>dv</code> in function names</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>8</p></td>
<td><p>Column <code>PRED</code> refers to population level predictions; rendered <code>pred</code> in function names</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>9</p></td>
<td><p>Column <code>IPRED</code> refers to individual level predictions; rendered <code>ipred</code> in function names</p></td>
</tr>
<tr class="even">
<td><p><strong>Plots generated</strong></p></td>
<td><p>10</p></td>
<td><p>Functions <code>dv_time</code>, <code>dv_tafd</code>, and <code>dv_tad</code> plot <code>DV</code> versus the appropriate time measure; both lines and points are plotted</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>11</p></td>
<td><p>Functions <code>dv_pred</code> and <code>dv_ipred</code> plot <code>DV</code> versus the appropriate predicted value; a line of identity is added as well as a loess smothing line; both the x- and y-axis maybe be transoformed to log scale with the <code>loglog</code> argument; if <code>loglog</code> is used, only positive values are retained for the plot</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>12</p></td>
<td><p>Functions <code>res_time</code>, <code>res_tafd</code>, and <code>res_tad</code> plots residual versus the appropriate time measure; a reference line is added at <code>res=0</code> as well as a loess smoothing line</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>13</p></td>
<td><p>Functions <code>wres_time</code>, <code>wres_tafd</code>, and <code>wres_tad</code> plots weighted residual versus the appropriate time measure; a reference line is added at <code>wres=0</code> as well as a loess smoothing line</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>14</p></td>
<td><p>Functions <code>cwres_time</code>, <code>cwres_tafd</code>, and <code>cwres_tad</code> plots conditional weighted residual versus the appropriate time measure; a reference line is added at <code>cwres=0</code> as well as a loess smoothing line</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>15</p></td>
<td><p>Functions <code>res_pred</code>, <code>wres_pred</code> and <code>cwres_pred</code> plot the appropriate residual versus population model predictions (<code>PRED</code>); a horizontal reference line is added at <code>c/w/res=0</code> as well as a loess smoothing line</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>16</p></td>
<td><p>Functions <code>res_cont</code>, <code>wres_cont</code>, and <code>cwres_cont</code> plot the appropriate residual versus a continuous covariate in the data set; a horizontal reference line is added at <code>c/w/res=0</code> as well as a loess smoothing line</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>17</p></td>
<td><p>Functions <code>res_cat</code>, <code>wres_cat</code>, and <code>cwres_cat</code> makes a boxplot of the appropriate residual versus a categorical data set column</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>18</p></td>
<td><p>Function <code>wres_q</code> and <code>cwres_q</code> generates quantile-quantile plots of the appropriate residual value; a refereince identity line is added</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>19</p></td>
<td><p>Function <code>eta_hist</code> generates histograms of model ETAs and returns a list <code>gg/ggplot</code> objects</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>20</p></td>
<td><p>Function <code>eta_cont</code> generates a scatterplot of model ETAs versus a continuous variable in the data set; a horizontal reference line at <code>ETAn=0</code> and loess smoothing line are also added to the plot</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>21</p></td>
<td><p>Function <code>eta_cat</code> generates boxplot summaries of model ETAs by a categorical variable in the data set</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>22</p></td>
<td><p>Function <code>eta_pairs</code> generates pairs plots using the <code>ggpairs</code> function from the <code>GGally</code> package</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>23</p></td>
<td><p>Function <code>splitplot</code> splits the input data set according to a discrete data item and generates a plot according to a user-named function, returning a list of <code>gg/ggplot</code> objects</p></td>
</tr>
<tr class="even">
<td><p><strong>Continuous scatter</strong></p></td>
<td><p>24</p></td>
<td><p>x-axis options availabe in <code>x_scale_continuous</code> can be modified by the xs argument</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>25</p></td>
<td><p>y-axis options available in <code>y_scale_continuous</code> can be modified by the ys argument</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>26</p></td>
<td><p>When loess smoothing lines are generated, <code>geom_smooth</code> with <code>ggplot2</code> default behavior is used; the smooth may be modified through the <code>smooth</code> argument</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>27</p></td>
<td><p>A title may be added through the <code>title</code> argument</p></td>
</tr>
<tr class="even">
<td><p><strong>Boxplot summaries</strong></p></td>
<td><p>28</p></td>
<td><p>x-axis options availabe in <code>x_scale_discrete</code> can be modified by the xs argument</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>29</p></td>
<td><p>y-axis options available in <code>y_scale_continuous</code> can be modified by the ys argument</p></td>
</tr>
<tr class="even">
<td></td>
<td><p>30</p></td>
<td><p>Boxplot summaries are generated using <code>geom_boxplot</code> with <code>ggplot2</code> default configuration</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>31</p></td>
<td><p>A title may be added thought the <code>title</code> argument</p></td>
</tr>
<tr class="even">
<td><p><strong>Input data</strong></p></td>
<td><p>32</p></td>
<td><p>Data are input as data.frame or tibble</p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>33</p></td>
<td><p>For continuous scatter plots, numeric data are required or an error is generated; data are considered discrete if they are <code>numeric</code> or <code>integer</code></p></td>
</tr>
<tr class="even">
<td></td>
<td><p>34</p></td>
<td><p>For boxplot summaries, discrete data are required for x-axis for boxplot summaries; data are considered discrete of they are <code>character</code>, <code>factor</code>, or <code>logical</code></p></td>
</tr>
<tr class="odd">
<td><p><strong>R packages</strong></p></td>
<td><p>35</p></td>
<td><p>Imports: <code>dplyr</code> <code>(&gt;= 0.7.2)</code>, <code>rlang</code> <code>(&gt;= 0.1.2)</code></p></td>
</tr>
<tr class="even">
<td></td>
<td><p>36</p></td>
<td><p>Depends: <code>ggplot2</code> <code>(&gt;= 2.2.1)</code></p></td>
</tr>
<tr class="odd">
<td></td>
<td><p>37</p></td>
<td><p>Suggests: <code>testthat</code></p></td>
</tr>
</tbody>
</table>
