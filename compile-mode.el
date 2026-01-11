;;; compile-mode.el --- Простой модуль для работы с режимами сборки

(defvar compile-build-mode "debug"
  "Текущий режим сборки для компиляции.")

(defun compile-set-build-mode (mode)
  "Установить режим сборки для компиляции."
  (interactive
   (list (completing-read "Режим сборки: " '("debug" "release") nil t compile-build-mode)))
  (setq compile-build-mode mode)
  (message "Режим сборки установлен: %s" mode))

(defun make-with-build-mode (mode)
  "Выполнить make с указанным режимом сборки."
  (interactive
   (list (completing-read "Режим сборки: " '("debug" "release") nil t "debug")))
  (let ((cmd (format "make BUILD_MODE=%s" mode)))
    (compile cmd)))

;; Добавляем в режим компиляции
(add-hook 'compilation-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c b") 'compile-set-build-mode)))

(provide 'compile-mode)
