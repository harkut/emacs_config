;; Конфигурация для atmega328p

(mcu-debug-register-target
 "atmega328p"
 '(:gdb-executable "avr-gdb"
   :gdb-args ""
   :simulator "simavr"
   :simulator-args "-g -m atmega328p"
   :uart-debugger "avrdude"
   :uart-args "-c arduino -p atmega328p -P /dev/ttyUSB0 -b 57600"
   :elf-suffix ".elf"
   :hex-suffix ".hex"
   :flash-make-target "flash"))
