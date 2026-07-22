# lem-tramp

Transparent remote file editing for Lem. Works like
[Emacs TRAMP](https://www.gnu.org/software/tramp/) — type a remote path
into `C-x C-f` and edit the file as if it were local.

## Supported Methods

| Method | Syntax | Description |
|--------|--------|-------------|
| `ssh`  | `/ssh:user@host:/remote/path` | Edit files on a remote host via SSH |
| `sudo` |     `/sudo::/local/path`      | Edit local files with root privileges via sudo |

## Usage

Open a file with `C-x C-f` using the TRAMP path syntax:

```
C-x C-f /ssh:root@example.com:/etc/nginx/nginx.conf
C-x C-f /sudo::/etc/hostname
```

The remote file is loaded into a buffer.  Edit normally, then save with `C-x C-s`.

## Authentication

### SSH (`/ssh:`)

**Key-based auth** (recommended) — no prompt, works automatically if you have
SSH keys set up and your key is in the remote host's `authorized_keys`.

**Password auth** — requires the `sshpass` utility:

```bash
# Arch
sudo pacman -S sshpass
# Debian/Ubuntu
sudo apt install sshpass
```

When key auth is unavailable, a password prompt appears in the minibuffer.
The password is cached for the session; subsequent file operations on the
same host reuse it without re-prompting.

### Sudo (`/sudo::`)

A password prompt appears if your sudo timestamp has expired (typically
5-15 minutes since your last `sudo` invocation in a terminal).  If you
have passwordless sudo configured (`NOPASSWD` in sudoers), no prompt is
shown.

## How It Works

lem-tramp hooks into Lem's virtual filesystem layer:

- **Reading** — runs `cat /remote/path` via SSH (or sudo), loads the
  output directly into an in-memory buffer.
- **Writing** — pipes the buffer content through `cat > /remote/path`.
- **Directory listing** — uses `ls -1a` for completion and directory-mode.
- **Metadata** — uses `stat -c '%s %Y'` for file size and modification time.

No temporary files are created on either the local or remote side.

## Dependencies

- **sshpass** — only needed for SSH password authentication
- **flexi-streams**, **str**, **babel**, **ppcre** — Common Lisp libraries

## Performance

SSH connections use `ControlMaster` multiplexing — the first command
establishes the TCP connection, and all subsequent commands reuse it.
A 5-second filesystem cache eliminates redundant `test -d` / `test -f` /
`stat` calls when Lem probes the same path multiple times during a single
file-open operation.
