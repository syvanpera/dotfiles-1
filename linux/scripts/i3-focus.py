#!/usr/bin/env python2

import subprocess
from xdo import Xdo

xdo = Xdo()
window = xdo.get_active_window()
window_pid = xdo.get_pid_window(window)
window_name = xdo.get_window_name(window)
active_process_pid = subprocess.check_output(['pgrep', '-P', "%s" % window_pid]).split('\n')[0]

def get_pname(id):
    p = subprocess.Popen(["ps -el |grep {} |grep -v zsh |grep ' [R|S] '".format(id)], stdout=subprocess.PIPE, shell=True)
    return str(p.communicate()[0])

active_process_name = get_pname(active_process_pid)
print(active_process_name)

subprocess.call(['dunstify', "Active window %i %s" % (window, active_process_name)])

if 'vim' in active_process_name:
    subprocess.call(['dunstify', 'VIM'])
elif 'Emacs' in active_process_name:
    subprocess.call(['dunstify', 'Emacs'])

