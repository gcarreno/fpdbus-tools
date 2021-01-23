# fpdbus-tools

## Free Pascal DBus Wrapper and Code Generation Tools

The author of this code is [Michaël Van Canneyt](mailto:michael@freepascal.org).

You can find the code and the article he wrote about it [here](https://www.freepascal.org/~michael/articles/#dbus2).

That page contains a real treasure trove of information and source code.

I'm just re-arranging the code and adding some niceaties.

## What does it contain?

The code is comprised of a set of pascal units that implement a Component wrapper around [DBus](https://dbus.freedesktop.org/):

- `src/dbuscomp.pp` DBUS component layer on top of the DBUS library.
- `src/dbusintf.pp` DBUS standard interfaces: Introspectable, DBUS daemon and Properties.
- `src/dbusintro.pp` DBUS Introspection and code generation components.
- `src/dbusproxy.pp` DBUS proxy component.

The code also includes a set of code generation tools, both in GUI and CLI format:

- `gui/fpdbusview` GUI tool for exploring and exporting code for both System and Session services.
- `cli/dbus2pas` CLI tools for exporting code for both System and Session services.

## fpdbusview

This gui tool depends on:
- LCL
- SynEdit (For the preview of the generated code)

(needs explanation of what is, what it does and how it does it)

## dbus2pas

This cli util has no dependencies in terms of packages.

This is a command line utility that you can use in scripting.
It does not have the visual aid to browse your exposed DBus services.
You will have done so with a DBus debug tool like the above mentioned `fpdbusview` or [D-Feet](https://wiki.gnome.org/Apps/DFeet).
Once you have the necessary information, you can use `dbus2pas` to generate ObjectPascal code to wrap around the service and ease it's use via a more OOP way.

Here are the options to use dbus2pas.

```man
Usage: dbus2pas options

    -h | --help                 print this help message
    -s | --system               connect to system bus
    -p | --objectpath=O         object to introspect in service (default is /)
    -o | --output=N             filename to write to (defaults to unitname)
    -d | --destination=D        destination service to connect to
    -f | --file=N               file to read XML from
    -u | --unitname=N           unitname (equals output if not set)
    -c | --codeoptions=N        Set code generation options. Comma-separated list of:
        GenerateInterface       Generate interface declaration
        GenerateProxy           Generate proxy declaration
        ProxyHasInterface       Proxy implements interface
        ProxyUsesProtected      Proxy methods are protected
        UseFunction             Use functions for methods with OUT parameter
        IncludeSystemInterfaces Include system interfaces
        LastPartInterfaceName   InterfaceName is constructed from last part of DBUS name if none specified
    -k | --keywordprefix=p      Prefix for pascal identifier names
    -x | --xmlfile=N            Write introspection XML to file (only with -d)
```

### Examples

```shell
dbus2pas -p /org/freedesktop/Notifications \
    -o org_freedesktop_Notifications.pas \
    -d org.freedesktop.Notifications \
    -u org_freedesktop_Notifications \
    -c GenerateInterface,GenerateProxy,UseFunction,LastPartInterfaceName
```
This will create a file called `org_freedesktop_Notifications.pas` containing code with an Interface and a Proxy to the org.freedesktop.Notifications service.
