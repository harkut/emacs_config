(provide '.emacs.flycheck)
(add-to-list 'flycheck-gcc-include-path
             (concat (substring
              (shell-command-to-string "ghc --print-libdir") 0 -1)
               "/include"))
(add-to-list 'flycheck-gcc-include-path (getenv "SPRJ"))


(add-to-list 'flycheck-gcc-definitions "DEBUG=3")
(add-to-list 'flycheck-gcc-definitions "_DEBUG")
(add-to-list 'flycheck-gcc-definitions "kVersion=0")
(add-to-list 'flycheck-gcc-definitions "kReleaseDate=0")

(add-to-list 'flycheck-gcc-warnings "all")
(setq flycheck-gcc-pedantic t)
;; (setq flycheck-gcc-language-standard "c++17")
(setq flycheck-gcc-args "-std=c++17")


(setq flycheck-checker-error-threshold 700)
