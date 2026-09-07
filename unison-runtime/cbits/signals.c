/* System.Posix.Signals saves Haskell handlers, but not arbitrary native
 * sigactions (including those installed by the RTS and curses). Keep the
 * complete original action so subscription cleanup can restore it too. */
#include <signal.h>
#include <stdlib.h>

#define SIGNAL_ENTRY(name) {#name, name}
static const struct { const char *name; int number; } signals[] = {
  SIGNAL_ENTRY(SIGHUP), SIGNAL_ENTRY(SIGINT), SIGNAL_ENTRY(SIGQUIT),
  SIGNAL_ENTRY(SIGALRM), SIGNAL_ENTRY(SIGTERM), SIGNAL_ENTRY(SIGUSR1),
  SIGNAL_ENTRY(SIGUSR2), SIGNAL_ENTRY(SIGCHLD), SIGNAL_ENTRY(SIGCONT),
  SIGNAL_ENTRY(SIGTSTP), SIGNAL_ENTRY(SIGTTIN), SIGNAL_ENTRY(SIGTTOU),
  SIGNAL_ENTRY(SIGURG), SIGNAL_ENTRY(SIGXCPU), SIGNAL_ENTRY(SIGXFSZ),
  SIGNAL_ENTRY(SIGPROF), SIGNAL_ENTRY(SIGVTALRM), SIGNAL_ENTRY(SIGWINCH),
#ifdef SIGPOLL
  SIGNAL_ENTRY(SIGPOLL),
#endif
#ifdef SIGIO
  SIGNAL_ENTRY(SIGIO),
#endif
#ifdef SIGINFO
  SIGNAL_ENTRY(SIGINFO),
#endif
#ifdef SIGPWR
  SIGNAL_ENTRY(SIGPWR),
#endif
};

int unison_signal_count(void) { return sizeof(signals) / sizeof(signals[0]); }
const char *unison_signal_name(int index) { return signals[index].name; }
int unison_signal_number(int index) { return signals[index].number; }

void *unison_signal_save(int signal) {
  struct sigaction *action = malloc(sizeof(*action));
  if (action == NULL) return NULL;
  if (sigaction(signal, NULL, action) != 0) {
    free(action);
    return NULL;
  }
  return action;
}

int unison_signal_restore(int signal, const void *action) {
  return sigaction(signal, action, NULL);
}
