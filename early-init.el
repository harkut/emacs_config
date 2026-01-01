;;; early-init.el --- Early initialization to speed up startup

;; Временно отключаем сборку мусора на время запуска
(setq gc-cons-threshold most-positive-fixnum) ; ~1 ГБ
(setq gc-cons-percentage 0.6)

;; Восстановим нормальные значения после запуска
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1024 1024)  ; 50 МБ
                  gc-cons-percentage 0.1)))

;; Отключаем ненужные элементы интерфейса до загрузки init.el
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      initial-major-mode 'fundamental-mode)

;; Ускоряем отрисовку
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; Увеличиваем буфер для вывода подпроцессов
(setq read-process-output-max (* 1024 1024)) ; 1 МБ

;; Отключаем обработчики имен файлов (ускоряет загрузку)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Восстанавливаем после запуска
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))

;; Предотвращаем раннюю инициализацию package.el
(setq package-enable-at-startup nil)

;;; early-init.el ends here
