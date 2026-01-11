# Корневой Makefile для AVR bare-metal проектов
PROJECT_NAME = avr_bare_metal
PROJECT_DIRS = $(wildcard */)  # Все поддиректории

# Фильтруем, чтобы исключить служебные директории
EXCLUDE_DIRS = doc .git
PROJECT_DIRS := $(filter-out $(EXCLUDE_DIRS), $(PROJECT_DIRS))

.PHONY: all clean doc flash check help $(PROJECT_DIRS)

# По умолчанию собираем все проекты
all:
	@echo "Building all projects..."
	@for dir in $(PROJECT_DIRS); do \
		if [ -f "$$dir/Makefile" ]; then \
			echo "=== Building $$dir ==="; \
			$(MAKE) -C $$dir || exit 1; \
		fi; \
	done

# Сборка конкретного проекта
$(PROJECT_DIRS):
	@if [ -f "$@/Makefile" ]; then \
		echo "=== Building $@ ==="; \
		$(MAKE) -C $@; \
	else \
		echo "Warning: No Makefile in $@"; \
	fi

# Статический анализ всех проектов
check:
	@echo "Running static analysis for all projects..."
	@for dir in $(PROJECT_DIRS); do \
		if [ -f "$$dir/Makefile" ]; then \
			echo "=== Checking $$dir ==="; \
			$(MAKE) -C $$dir check || true; \
		fi; \
	done

# Глобальная документация
doc:
	@echo "Generating global documentation..."
	@mkdir -p doc
	@if [ -f Doxyfile ]; then \
		doxygen Doxyfile; \
		if [ -d "doc/latex" ]; then \
			$(MAKE) -C doc/latex >/dev/null 2>&1; \
			cp doc/latex/refman.pdf doc/$(PROJECT_NAME)_full.pdf; \
			echo "Global PDF created: doc/$(PROJECT_NAME)_full.pdf"; \
		fi; \
	else \
		echo "Warning: Root Doxyfile not found."; \
	fi

# Очистка всех проектов
clean:
	@echo "Cleaning all projects..."
	@for dir in $(PROJECT_DIRS); do \
		if [ -f "$$dir/Makefile" ]; then \
			$(MAKE) -C $$dir clean || true; \
		fi; \
	done
	rm -rf doc

# Справка
help:
	@echo "AVR Bare-metal Projects Root Makefile"
	@echo "======================================"
	@echo "Available targets:"
	@echo "  all      : Build all projects"
	@echo "  clean    : Clean all projects"
	@echo "  doc      : Generate global documentation"
	@echo "  check    : Run static analysis on all projects"
	@echo "  help     : Show this help"
	@echo ""
	@echo "Available projects:"
	@for dir in $(PROJECT_DIRS); do \
		echo "  $$dir"; \
	done
