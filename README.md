# rusty-chips

CHIP 8 Emulator in Rust

## Assembler Syntax

The syntax consists of a series of statements describing the program.

### Statements

A statement consists of tokens separated by whitespace, terminated by either a
newline or semicolon. Empty statements are allowed.

A typical statement consists of:

- [Label](#labels) (Optional)
- [Instruction](#instructions) or [Directive](#directives)
- [Operands](#operands)
- [Comment](#comments) (Optional)
- Semicolon or newline

### Labels

A label consists of an identifier followed by a colon. An identifier can be any
alphanumeric character or an `_`.

#### Symbolic Labels
Symbolic labels must start with either a letter or underscore. They can only be
defined once per scope (global or local).

Symbolic labels not starting with an `_` are considered global and are included
in the symbol table. Symbolic labels staring with an `_` are considered to have
local scope and are not included in the symbol table.

#### Numeric Labels
Numeric labels consist of an integer between 0 and 99 (inclusive). They are
always considered local and are not included in the symbol table.

When referencing a numeric label the suffixes `b` (backwards) or `f` (forwards)
shall be appended. This indicates the direction that a references should be
searched for. For example `4b` would refer to the nearest label `4` used before
the reference.

### Instructions

### Directives

#### `.start`
Set the current cursor to 0x200 which is the start location of a Chip-8 program.

### Operands

| Mode     | Format | Operand 1                 | Operand 2          |
| -------- | ------ | ------------------------- | ------------------ |
| none     | 0000   |                           |                    |
| short    | 0nnn   | Literal or label, 12 bits |                    |
| byte_reg | 0snn   | Literal byte, 8 bits      | Register s, 4 bits |

### Comments

Comments can appear at the end of a statement or on their own. They consist of
the `#` character followed by the text of the comment. They are terminated by a
newline.
