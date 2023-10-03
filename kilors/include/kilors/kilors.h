#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct editorSyntax {
  char **filematch;
  char **keywords;
  char singleline_comment_start[2];
  char multiline_comment_start[3];
  char multiline_comment_end[3];
  int flags;
} editorSyntax;

/**
 * This structure represents a single line of the file we are editing.
 */
typedef struct erow {
  /**
   * Row index in the file, zero-based.
   */
  int idx;
  /**
   * Size of the row, excluding the null term.
   */
  int size;
  /**
   * Size of the rendered row.
   */
  int rsize;
  /**
   * Row content.
   */
  char *chars;
  /**
   * Row content "rendered" for screen (for TABs).
   */
  char *render;
  /**
   * Syntax highlight type for each character in render.
   */
  unsigned char *hl;
  /**
   * Row had open comment at end in last syntax highlight check.
   */
  int hl_oc;
} erow;

void use_editorSyntax(const struct editorSyntax*);

void use_erow(const struct erow*);
