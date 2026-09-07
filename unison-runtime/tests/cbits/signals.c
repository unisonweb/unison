#include <signal.h>
#include <stdlib.h>

static volatile sig_atomic_t notifications;
static void native_handler(int signal) { (void)signal; notifications++; }
struct saved_action { int signal; struct sigaction action; };

void *unison_test_signal_begin(int signal) {
  struct saved_action *saved = malloc(sizeof(*saved));
  if (!saved) return NULL;
  saved->signal = signal;
  struct sigaction action = {0};
  action.sa_handler = native_handler;
  action.sa_flags = SA_RESTART;
  sigemptyset(&action.sa_mask);
  sigaddset(&action.sa_mask, SIGHUP);
  notifications = 0;
  if (sigaction(signal, &action, &saved->action) != 0) {
    free(saved);
    return NULL;
  }
  return saved;
}

void unison_test_signal_end(void *pointer) {
  if (!pointer) return;
  struct saved_action *saved = pointer;
  sigaction(saved->signal, &saved->action, NULL);
  free(saved);
}

int unison_test_signal_restored(int signal) {
  struct sigaction action;
  if (sigaction(signal, NULL, &action) != 0) return 0;
  return action.sa_handler == native_handler &&
    (action.sa_flags & SA_RESTART) && sigismember(&action.sa_mask, SIGHUP);
}

int unison_test_signal_count(void) { return notifications; }
