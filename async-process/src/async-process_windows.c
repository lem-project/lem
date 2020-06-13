#include "async-process.h"
#ifdef HAVE_WINDOWS_H

__declspec(dllexport)
struct process* create_process(char *const command[], bool nonblock, const char *path)
{
  struct process* ret=malloc(sizeof(struct process));
  HANDLE hErrorWrite = INVALID_HANDLE_VALUE;
  HANDLE hOutputReadTmp = INVALID_HANDLE_VALUE;
  HANDLE hOutputWrite = INVALID_HANDLE_VALUE;
  HANDLE hInputWriteTmp = INVALID_HANDLE_VALUE;
  HANDLE hInputRead = INVALID_HANDLE_VALUE;

  SECURITY_ATTRIBUTES sa;
  ret->nonblock=nonblock;
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = 0;
  sa.bInheritHandle = TRUE;

  HANDLE currproc = GetCurrentProcess();

  if (!CreatePipe(&hOutputReadTmp, &hOutputWrite, &sa, 0))
    return NULL;
  if (!DuplicateHandle(currproc, hOutputWrite, currproc, &hErrorWrite, 0, TRUE, DUPLICATE_SAME_ACCESS))
    return NULL;
  if (!CreatePipe(&hInputRead, &hInputWriteTmp, &sa, 0))
    return NULL;
  if (!DuplicateHandle(currproc, hOutputReadTmp, currproc, &(ret->hOutputRead), 0, FALSE, DUPLICATE_SAME_ACCESS))
    return NULL;
  if (!DuplicateHandle(currproc, hInputWriteTmp, currproc, &(ret->hInputWrite), 0, FALSE, DUPLICATE_SAME_ACCESS))
    return NULL;

  CloseHandle(hOutputReadTmp);
  CloseHandle(hInputWriteTmp);

  STARTUPINFOA si;

  ZeroMemory(&si, sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  si.dwFlags = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  si.wShowWindow = SW_HIDE;
  si.hStdInput = hInputRead;
  si.hStdOutput = hOutputWrite;
  si.hStdError = hErrorWrite;
  {
    char *tmp;
    int size = 0;
    for(int i=0;command[i]!=NULL;++i)
      size+=strlen(command[i])+1;
    tmp=alloca(size);
    for(int i=0,j=0,k=0;;++j,++k) {
      if(command[i][j]=='\0') {
	j=-1;
	if(command[++i]) {
	  tmp[k]=' ';
	}else {
	  tmp[k]='\0';
	  break;
	}
      }else
	tmp[k]=command[i][j];
    }
    if (!CreateProcessA(0, tmp, 0, 0, TRUE, CREATE_NO_WINDOW, 0, path, &si, &(ret->pi)))
      return NULL;
  }
  CloseHandle(hOutputWrite);
  CloseHandle(hInputRead);
  CloseHandle(hErrorWrite);

  return ret;
}

__declspec(dllexport)
void delete_process(struct process *process)
{
  TerminateProcess(process->pi.hProcess,2);
  CloseHandle(process->hInputWrite);
  CloseHandle(process->hOutputRead);
  CloseHandle(process->pi.hThread);
  free(process);
}

__declspec(dllexport)
int process_pid(struct process *process)
{
  return process->pi.dwProcessId;
}

__declspec(dllexport)
void process_send_input(struct process *process, const char *string)
{
  DWORD n = 0;
  WriteFile(process->hInputWrite,string,strlen(string),&n,NULL);
}

__declspec(dllexport)
const char* process_receive_output(struct process *process)
{
  DWORD n = 0;
  DWORD avail;
  if(process->nonblock) {
    if (!PeekNamedPipe (process->hOutputRead, 0, 0, 0, &avail, 0))
      return NULL;
    if(!avail)
      return NULL;
  }
  if (ReadFile(process->hOutputRead, process->buffer, sizeof(process->buffer)-1, &n, NULL)) {
    process->buffer[n] = '\0';
    return process->buffer;
  }
  return NULL;
}

__declspec(dllexport)
int process_alive_p(struct process *process)
{
  DWORD dwExitCode;
  GetExitCodeProcess(process->pi.hProcess, &dwExitCode);
  return (dwExitCode == STILL_ACTIVE);
}
#endif
