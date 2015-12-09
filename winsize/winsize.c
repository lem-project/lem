#include <sys/ioctl.h>
#include <unistd.h>

void win_size(int fd, int *row, int *col)
{
	struct winsize ws;
	if (ioctl(fd, TIOCGWINSZ, &ws) == 0) {
		*row = ws.ws_row;
		*col = ws.ws_col;
	} else {
		*row = -1;
		*col = -1;
	}
}

int win_row(int fd)
{
	int row, col;
	win_size(fd, &row, &col);
	return row;
}

int win_col(int fd)
{
	int row, col;
	win_size(fd, &row, &col);
	return col;
}
