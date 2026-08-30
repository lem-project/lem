# Ansible Mode for Lem

`ansible-mode` is a major mode for editing Ansible playbooks, tasks, roles, and configuration files in the Lem editor. It extends `yaml-mode` with automatic file detection and Language Server Protocol (LSP) support.

## Features

- **YAML Syntax & Formatting**: Inherits YAML syntax highlighting, indentation settings (2 spaces, tabs disabled), and line comment rules (`#`).
- **Automatic File Detection**: Automatically detects Ansible files and switches the buffer mode when opening or saving YAML files matching Ansible patterns.
- **Language Server Protocol (LSP)**: Built-in integration with `ansible-language-server` for autocompletion, validation, hover documentation, and syntax diagnostics.

## Requirements

To use Language Server features, install the official Ansible Language Server:

```bash
npm install -g @ansible/ansible-language-server
```

For more details on the language server, see the [Ansible Language Server Documentation](https://docs.ansible.com/projects/vscode-ansible/als/).

## Usage

### Automatic Activation

When opening a YAML file (`.yaml` or `.yml`), `ansible-mode` automatically inspects the first 30 lines. If standard Ansible directives (such as `- hosts:`, `- name:`, `tasks:`, `import_playbook:`, or `ansible.builtin.*`) are detected, the buffer is automatically switched to `ansible-mode`.

The detection also runs before saving a file to ensure newly authored playbooks transition to `ansible-mode`.

### Manual Activation

You can activate `ansible-mode` manually in any buffer:

```
M-x ansible-mode
```

### Enabling LSP

Once `ansible-language-server` is installed on your system, start LSP support in the current buffer:

```
M-x lsp-mode
```

To automatically start LSP whenever `ansible-mode` is activated, add a hook to your initialization file.

## Configuration

Add the following to your Lem configuration file (`~/.lem/init.lisp` or `~/.config/lem/init.lisp`):

### Automatic LSP Activation

```lisp
(add-hook lem-ansible-mode:*ansible-mode-hook*
          'lem-lsp-mode:lsp-mode)
```

### Custom Keybindings

You can bind mode-specific commands using `*ansible-mode-keymap*`:

```lisp
(define-key lem-ansible-mode:*ansible-mode-keymap* "C-c C-k" 'your-custom-command)
```

### Custom Mode Hooks

You can run custom logic when entering `ansible-mode`:

```lisp
(add-hook lem-ansible-mode:*ansible-mode-hook*
          (lambda ()
            (setf (variable-value 'tab-width :buffer (current-buffer)) 2)))
```

## File Detection Details

Ansible mode inspects the first 30 lines of any buffer in `yaml-mode` matching the following regex pattern:

```regex
^\s*(-\s+(name|hosts|import_playbook|task|tasks)\s*:|ansible\.builtin\.\S*)
```

Matching files are automatically transitioned from `yaml-mode` to `ansible-mode`.
