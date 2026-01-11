;;; atmega328p.el --- Конфигурация для ATmega328P

;; Регистрируем цель
(mcu-debug-register-target
 "atmega328p"
 '(:gdb-executable "avr-gdb"
   :gdb-args ""
   :simulator "simavr"
   :simulator-args "-m atmega328 -f 16000000 -g"
   :uart-debugger "avrdude"
   :uart-args "-c arduino -p atmega328p -P /dev/ttyUSB0 -b 115200"
   :elf-suffix ".elf"
   :hex-suffix ".hex"
   :flash-make-target-debug "flash"
   :flash-make-target-release "flash-release"
   :debug-cflags "-g -Og"
   :release-cflags "-Os -flto"
   :make-variables ((MCU . "atmega328p")
                    (F_CPU . 16000000)
                    (CC . "avr-gcc")
                    (OBJCOPY . "avr-objcopy")
                    (OBJDUMP . "avr-objdump")
                    (SIZE . "avr-size")
                    (AVRDUDE . "avrdude")
                    (PROGRAMMER_TYPE . "arduino")
                    (PROGRAMMER_BAUD . 57600))
   :memory-map ((flash :start 0x0 :size 0x8000)
                (sram  :start 0x100 :size 0x800))))

(provide 'atmega328p)
