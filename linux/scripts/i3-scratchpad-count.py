#!/usr/bin/env python3

import i3ipc

i3 = i3ipc.Connection()

def scratchpad_count():
    leaves = i3.get_tree().leaves()
    scratch_count = 0
    for con in leaves:
        if con.workspace().name == '__i3_scratch':
            scratch_count += 1

    return scratch_count

# Define a callback to be called when you switch workspaces.
def on_window(self, e):
    # The first parameter is the connection to the ipc and the second is an object
    # with the data of the event sent from i3.
    if e.current:
        print('Windows on this workspace:')
        for w in e.current.leaves():
            print(w.name)

def on_window_move(i3, e):
    print(scratchpad_count())

# Subscribe to events
i3.on("window::move", on_window_move)

print(scratchpad_count())

# Start the main loop and wait for events to come in.
i3.main()
