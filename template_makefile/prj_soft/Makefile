# Корневой Makefile шаблона
PROJECT_NAME = eltex_chapter_embedded_c
WORK_DIRS    = $(wildcard work_*)

.PHONY: all clean doc $(WORK_DIRS)

all: $(WORK_DIRS)

# Рекурсивный обход папок разделов
$(WORK_DIRS):
	@$(MAKE) -C $@

# Глобальная документация по всем модулям и упражнениям
doc:
	@echo "--- Generating Global Documentation ---"
	@mkdir -p doc
	@if [ -f Doxyfile ]; then \
		doxygen Doxyfile; \
		if [ -d "doc/latex" ]; then \
			$(MAKE) -C doc/latex; \
			cp doc/latex/refman.pdf doc/$(PROJECT_NAME)_full.pdf; \
		fi; \
	else \
		echo "Error: Root Doxyfile not found."; \
	fi

clean:
	@for dir in $(WORK_DIRS); do $(MAKE) -C $$dir clean; done
	rm -rf doc
