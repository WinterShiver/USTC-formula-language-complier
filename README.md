# 中科大信院 编译原理课设：数学公式的排版

本项目是中科大信院编译原理课程的课程实验的Haskell实现，将LaTeX语法的公式文本渲染成HTML. 

![test_effect](https://github.com/WinterShiver/USTC-formula-language-complier/raw/master/fig/test_effect.png)

⭐**求Star！**

## 文件说明

* readme.pdf是这个项目的要求。为了优化输出公式的视觉效果，在要求的基础上做了一点修改。
* report.md是这个项目的文档，解释了代码的编写思路和一些说明。
* code目录下为源代码。
* test目录下为源语言（公式字符串），result目录下为目标语言（得到的HTML文件）。错误的源语言句子会得到空的目标语言文档。

## 运行代码

在交互命令行运行：如图，进入`/code`目录，进入`ghci`环境，`:l Main`加载Main模块，`main`执行Main模块的main函数。

![run](https://github.com/WinterShiver/USTC-formula-language-complier/raw/master/fig/run.png)

编译并运行：进入`/code`目录，`ghc Main.hs && ./Main`

![compile](https://github.com/WinterShiver/USTC-formula-language-complier/raw/master/fig/compile.png)

