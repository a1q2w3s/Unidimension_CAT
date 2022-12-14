## Unidimension_CAT
<div align='center'>
  <img src='/CAT_demo.png' width=400>
  
  <sub>模拟生成的计算机自适应测验过程</sub>
</div><br>

本项目为陈平老师《自适应测验与自适应诊断评估》课程的期末习作，旨在通过R编写的模拟程序，实现**单维度**能力参数下的计算机自适应测验（computerized adaptive testing， CAT）。

代码存储于code文件夹中，包括主程序Unidimension_CAT.R和function文件夹中的测验组块Unidimension_function.R；此外script文件夹存储项目编写过程中的一些草稿。

### CAT组块

通常来说，计算机自适应测验可选的组块包括：
- 能力估计方法：如极大似然估计MLE、期望后验估计EAP和最大后验估计MAP

- 选题策略：
  - 初始题目选择：在测验开始时，受测者的能力参数未知，因此无法自适应选择题目，此时通常的做法是从题库中随机选择题目（可能会优先选择信息量大的题目、中等难度的题目）。当然，若有对于受测者能力的先验信息，如上次测验的成绩，也可以基于先验信息来选题。
  - 自适应题目选择：当受测者完成了初始题目后，我们就有了对于受测者能力的初步估计，此时就可以依据估计能力进行自适应的选题，常用的选题策略如最大费舍信息量方法、KL全局信息量方法等。

- 终止规则：如固定长度终止规则、估计标准误阈限终止规则、估计稳定性终止规则等。

本项目实现了三种能力估计方法（极大似然估计MLE、期望后验估计EAP和最大后验估计MAP），初始题目方法选择为从题库中随机选择5题，自适应题目选择策略为最大费舍信息量方法，终止规则为固定长度为40道题。能力估计方法中MLE和MAP的优化采用牛顿-拉夫逊迭代法进行。

### 主程序

本项目实现了两个不同功能的主程序。

程序一在自适应测验进行过程中，输出当前能力估计值和所选题目难度的线图，直观呈现自适应测验的过程。

程序二则考察了多种不同组块组合下，能力估计的表现与模拟自适应测验用时（结果如下图所示）。

<div align='center'>
  <img src='/RMSE.png' width=500>
  
  <sub>不同组块组合下能力估计的RMSE</sub>
</div><br>

<div align='center'>
  <img src='/runtime.png' width=500>
  
  <sub>不同组块组合下CAT用时</sub>
</div><br>

