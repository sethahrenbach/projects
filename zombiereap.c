#include <stdlib.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
// Seth Kurtenbach 
// #12049446
// CS4520
// Project 2

#define NCHILDREN 1024

pid_t children[NCHILDREN];


void execute(char *command[])
{
	int status = 0;
	pid_t pid;
	pid = fork();
	if (pid == 0) //if you're the child, execute the command, then die.
	{
		execvp(command[0], command);
	}
	//if you're the parent, wait for the child to finish, then return to being a shell.
	waitpid(pid, &status, 0);
	if (status != 0) 
	{
		fprintf (stderr, "error: %s broke due to code %d\n", *command, status);
	}
	printf("%d\n", status);
}

void background(char *command[]);

void chexecute(char *cmd[])
{
	int i = 0;
	printf("%s\n", cmd[i]);
	while (cmd[i] != 0)
	{
		i++;	
	}
	if (*cmd[i - 1] == '&') 
		background(cmd);
	else
	{
		printf("%s\n", cmd[i]);
		execute(cmd);
	}
}
void addpid(pid_t pid)
{
	int i=0;
// take a pid and stick it in the first empty slot in the array.
	while (children[i]!=0) i++;
	children[i] = pid;

}

void remvpid (int i)
{
// if a pid is 0, then the spot is empty.
	children[i]=0;

}

void reap()
{
//check to see if child is zombie, if so, wait on it. then go to the next child.
int i = 0;
int kidpid = 0;

	int status = 0;
	pid_t pid;
	while (i != NCHILDREN)
	{
		kidpid = waitpid(pid, &status, WNOHANG);
		if (kidpid > 0)
			remvpid (i);
		i++;	
	}
}




int main (int argc, char *argv[])
{
	char *input;
	char *token;
	while ((input = readline (">>>> ")) != NULL)
	{
		reap();
	
		token = strtok(input," \t\n");

		if (strcmp(token, "exit") == 0)
			exit(0);

		char **cmd;
		int i = 1;

		cmd = malloc(2 * (sizeof (char *)));
			
		cmd[0] = token;

		while (token != NULL)
		{
			token = strtok(NULL," \t\n");	
			cmd[i] = token;
			i++;
			cmd = realloc(cmd, (sizeof(char *) * (i + 1)));
		}
		cmd[i] = 0;
		chexecute(cmd);


	}		
	return 0;
}

