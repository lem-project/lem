#ifndef _ASYNC_PROCESS_H_
#define _ASYNC_PROCESS_H_

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif


#ifdef HAVE_WINDOWS_H
# include <windows.h>
#else
# define _GNU_SOURCE
# include <signal.h>
# include <errno.h>
# include <fcntl.h>
# include <sys/wait.h>
# include <termios.h>
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

struct process {
  char buffer[1024*4];
#ifdef HAVE_WINDOWS_H
  PROCESS_INFORMATION pi;
  HANDLE hInputWrite;
  HANDLE hOutputRead;
  bool nonblock;
#else
  int fd;
  char *pty_name;
  pid_t pid;
#endif
};

struct process* create_process(char *const command[], bool nonblock, const char *path);
void delete_process(struct process *process);
int process_pid(struct process *process);
void process_send_input(struct process *process, const char *string);
const char* process_receive_output(struct process *process);
int process_alive_p(struct process *process);

#endif
