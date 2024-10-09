;; -*- lexical-binding: t -*-
(use-package fanyi
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))

(use-package immersive-translate
  :config
  (setq immersive-translate-backend 'baidu
      immersive-translate-baidu-appid "20211214001027891"))

(provide 'init-translation)
