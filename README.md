# 高性能Emacs配置模板

这个配置采用了模块化设计，专注于性能优化，并包含了美观的Catppuccin Mocha主题和透明背景。

## 目录结构

```
.
├── early-init.el          # 启动前初始化（GUI禁用等）
├── init.el                # 主配置入口
├── config/                # 个人配置文件
│   └── custom.el          # 自定义设置
└── modules/               # 功能模块
    ├── core/              # 核心配置
    │   ├── core-init.el          # 基础设置
    │   ├── line-number-config.el # 行号配置
    │   ├── hl-line-config.el     # 当前行高亮配置
    │   └── smooth-scrolling-config.el # 高度优化的平滑滚动配置
    ├── ui/                # 界面配置
    │   ├── ui-init.el          # UI主配置
    │   ├── catppuccin-config.el # Catppuccin主题配置
    │   ├── transparent-bg.el    # 透明背景配置
    │   └── mode-line-config.el  # VS Code风格Mode line美化配置
    ├── lang/              # 语言支持
    │   └── lang-init.el   # 编程语言配置
    ├── tools/             # 工具配置
    │   ├── tools-init.el       # 工具主配置
    │   ├── which-key-config.el # 键盘快捷键提示配置
    │   └── git-config.el       # Git工具配置
    └── perf/              # 性能优化
        └── perf-init.el   # 启动和运行时优化
```

## 性能优化特性

1. **启动优化**
   - 禁用不必要的GUI元素
   - 延迟垃圾回收
   - 最小化初始帧大小

2. **运行时优化**
   - 智能语法高亮（大文件自动禁用）
   - 优化滚动和渲染
   - 减少不必要的更新

3. **包管理优化**
   - 使用ELPA和MELPA源
   - 按需加载模块

## 主题和外观

### Catppuccin Mocha主题
- 美观的深色主题
- 低对比度色彩方案，减少眼部疲劳
- 支持多种编程语言

### 透明背景
- 窗体背景完全透明
- 适用于各种桌面环境
- 可与任何壁纸完美融合

## 行号和高亮功能

### 行号显示
- 默认启用相对行号
- 支持绝对行号和相对行号切换
- 针对不同模式的优化配置

### 当前行高亮
- 全局启用当前行高亮
- 与Catppuccin主题完美融合
- 透明背景适配
- 可自定义高亮颜色和样式

## 平滑滚动功能
- 🚀 **智能加速**: 连续滚动自动加速
- 🎯 **动态调整**: 根据窗口大小智能调整滚动量
- ⚡ **自然体验**: 开始和结束慢，中间快的自然滚动感觉
- 🌊 **流畅动画**: 优化的延迟时间创造完美平滑效果
- 🛡️ **错误处理**: 完善的边界和异常处理
- 🎮 **响应迅速**: 即时响应用户输入

## 键盘快捷键提示 (Which-Key)
- 🎯 **实时提示**: 按键组合实时显示可用命令
- 📚 **自定义描述**: 为常用键绑定提供友好名称
- 🎨 **美观界面**: 底部弹出式提示窗口
- ⚡ **快速响应**: 0.5秒延迟快速显示
- 🔄 **智能分组**: 按功能对键绑定进行分组
- 🌐 **模式特定**: 支持不同模式的键绑定提示

## Git工具集成
- 🎯 **Magit**: 强大的Git接口，完整的Git操作支持
- 📊 **Git Gutter**: 在行号旁直观显示Git更改状态
- 🔗 **Browse at Remote**: 一键在浏览器中查看Git文件
- 🕐 **Git Timemachine**: 查看文件的历史版本
- 🔄 **Auto Revert**: 自动刷新Git更改
- 🌐 **Forge**: GitHub/GitLab集成支持

## Mode Line美化 (VS Code风格)
- 🎨 现代化VS Code风格设计
- 📄 文件类型图标支持
- 🌈 Catppuccin配色方案
- 📊 清晰的信息展示
- 🔧 状态指示器
- 📏 简洁布局无多余线条

## 使用方法

1. 将此目录放置在 `~/.emacs.d/`
2. 启动Emacs即可自动加载配置
3. 在 `config/custom.el` 中添加个人定制

## 模块说明

### core-init.el
核心配置包括编码、备份、历史记录等基础设置。

### line-number-config.el
行号显示的详细配置。

### hl-line-config.el
当前行高亮的详细配置。

### smooth-scrolling-config.el
高度优化的平滑滚动配置。

### ui-init.el
界面相关配置，包括主题、字体、模式行等。

### catppuccin-config.el
Catppuccin Mocha主题的详细配置，包括透明背景支持。

### transparent-bg.el
专门用于设置透明背景的配置文件。

### mode-line-config.el
VS Code风格Mode line美化的配置文件。

### tools-init.el
工具配置的主入口文件。

### which-key-config.el
键盘快捷键提示配置文件。

### git-config.el
Git工具配置文件。

### lang-init.el
编程语言支持，针对不同语言进行性能优化。

### perf-init.el
专门的性能调优设置，包括GC优化、启动时间测量等。# emacs
