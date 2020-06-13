#include "async-process.h"

#ifndef HAVE_WINDOWS_H

static const char* open_pty(int *out_fd)
{
  int fd = posix_openpt(O_RDWR | O_CLOEXEC | O_NOCTTY);
  if (fd < 0) return NULL;
  if (grantpt(fd) == -1 || unlockpt(fd) == -1) return NULL;
  fcntl(fd, F_SETFD, FD_CLOEXEC);
  const char *name = ptsname(fd);
  if (name == NULL) {
    close(fd);
    return NULL;
  }
  *out_fd = fd;
  return name;
}

static struct process* allocate_process(int fd, const char *pts_name, int pid)
{
  struct process *process = malloc(sizeof(struct process));
  if (process == NULL)
    return NULL;
  process->fd = fd;
  process->pty_name = malloc(strlen(pts_name) + 1);
  process->pid = pid;
  strcpy(process->pty_name, pts_name);
  return process;
}

struct process* create_process(char *const command[], bool nonblock, const char *path)
{
  int pty_master;
  const char *pts_name = open_pty(&pty_master);
  if (pts_name == NULL)
    return NULL;

  if (nonblock)
    fcntl(pty_master, F_SETFL, O_NONBLOCK);

  int pipefd[2];

  if (pipe(pipefd) == -1) return NULL;

  pid_t pid = fork();

  if (pid == 0) {
    close(pipefd[0]);
    pid = fork();
    if (pid == 0) {
      close(pipefd[1]);
      setsid();
      int pty_slave = open(pts_name, O_RDWR | O_NOCTTY);
      close(pty_master);

      // Set raw mode
      struct termios tty;
      tcgetattr(pty_slave, &tty);
      cfmakeraw(&tty);
      tcsetattr(pty_slave, TCSANOW, &tty);

      dup2(pty_slave, STDIN_FILENO);
      dup2(pty_slave, STDOUT_FILENO);
      dup2(pty_slave, STDERR_FILENO);
      close(pty_slave);
      if (path != NULL) chdir(path);
      execvp(command[0], command);
      int error_status = errno;
      if (error_status == ENOENT) {
        char str[128];
        sprintf(str, "%s: command not found", command[0]);
        write(STDIN_FILENO, str, strlen(str));
      } else {
        char *str = strerror(error_status);
        write(STDIN_FILENO, str, strlen(str));
      }
      exit(error_status);
    } else {
      char buf[12];
      sprintf(buf, "%d", pid);
      write(pipefd[1], buf, strlen(buf)+1);
      close(pipefd[1]);
      exit(0);
    }
  } else {
    close(pipefd[1]);
    if (waitpid(pid, NULL, 0) == -1)
      return NULL;
    char buf[12];
    read(pipefd[0], buf, sizeof(buf));
    close(pipefd[0]);
    return allocate_process(pty_master, pts_name, atoi(buf));
  }

  return NULL;
}

void delete_process(struct process *process)
{
  kill(process->pid, 9);
  close(process->fd);
  free(process->pty_name);
  free(process);
}

int process_pid(struct process *process)
{
  return process->pid;
}

void process_send_input(struct process *process, const char *string)
{
  write(process->fd, string, strlen(string));
}

const char* process_receive_output(struct process *process)
{
  int n = read(process->fd, process->buffer, sizeof(process->buffer)-1);
  if (n == -1)
    return NULL;
  process->buffer[n] = '\0';
  return process->buffer;
}

int process_alive_p(struct process *process)
{
  return kill(process->pid, 0) == 0;
}
#endif
