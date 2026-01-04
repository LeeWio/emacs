;;; early-init.el --- early startup config -*- lexical-binding: t; -*-

;; 禁用 package.el（避免和 straight.el 冲突）
(setq package-enable-at-startup nil)

;; 启动阶段少点垃圾
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; 减少 GC 压力（启动更快）
(setq gc-cons-threshold most-positive-fixnum)