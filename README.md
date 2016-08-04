# fpdbus-tools

##Free Pascal DBus and Code Generation Tools

The author of this code is [Michaël Van Canneyt](mailto:michael@freepascal.org).

I'm just re-arranging the code and adding some niceaties.

###What does it contain?

The code is comprised of a set of pascal units that implement a Component wrapper around [DBus](https://dbus.freedesktop.org/):

- `dbuscomp.pp` DBUS component layer on top of the DBUS library.
- `dbusintf.pp` DBUS standard interfaces: Introspectable, DBUS daemon and Properties.
- `dbusintro.pp` DBUS Introspection and code generation components.
- `dbusproxy.pp` DBUS proxy component.

The code also includes a set of code generation tools, both in GUI and CLI format:

- `fpdbusview` GUI tool for exploring and exporting code for both System and Session services.
- `dbus2pas` CLI tools for exporting code for both System and Session services.

###fpdbusview

(needs explanation of what is, what it does ans how it does it)

###dbus2pas

Here are the options to use dbus2pas.

```
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
