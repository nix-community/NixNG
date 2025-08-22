#include <stdlib.h>
#include <sys/types.h>
#include <grp.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <pwd.h>
#include <stdbool.h>

void set_user(char* username, bool set_group) {
  struct passwd* user = getpwnam(username);

  if (!user) {
    fprintf(stderr, "invalid user: %s\n", username);
    exit(1);
  }

  if (set_group) {
    if(setgid(user->pw_gid) != 0) {
      fprintf(stderr, "failed to switch group\n");
      exit(1);
    }
  }

  if (setuid(user->pw_uid) != 0) {
    fprintf(stderr, "failed to switch user\n");
    exit(1);
  }
}

void set_group(char* groupname) {
  struct group* group = getgrnam(groupname);

  if (!group) {
    fprintf(stderr, "invalid group: %s\n", groupname);
    exit(2);
  }

  if (setgid(group->gr_gid) != 0) {
    fprintf(stderr, "failed to switch group\n");
    exit(2);
  }
}

int main (int argc, char *argv[]) {
  if (argc < 5) {
    fprintf(stderr, "USAGE: setgroups <user> <group> <supplementary groups>");
    exit(255);
  }


  char* user = argv[1];
  char* group = argv[2];
  char* groups = strdup(argv[3]);

  char* svptr = NULL;
  size_t groups_count = 0;

  for (char* tok = strtok_r(groups, ":", &svptr); tok; tok = strtok_r(NULL, ":", &svptr)) {
    groups_count++;
  }
  free(groups);
  groups = strdup(argv[3]);

  gid_t* gids = calloc(sizeof(*gids), groups_count);

  svptr = NULL;
  size_t i = 0;
  for (char* tok = strtok_r(groups, ":", &svptr); tok; tok = strtok_r(NULL, ":", &svptr)) {
    struct group* group = getgrnam(tok);

    if (!group) {
      fprintf(stderr, "invalid group: %s\n", tok);
      exit(3);
    }

    gids[i] = group->gr_gid;
    i++;
  }

  if (strcmp(group, ":") != 0) {
    set_group(group);
  }
  setgroups(groups_count, gids);
  set_user(user, strcmp(group, ":") == 0);

  execvp(argv[4], &argv[4]);

  perror("cannot exec");
}
