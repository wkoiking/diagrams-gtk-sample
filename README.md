# How to Install

## Ubuntu

Check the Internet Connection.

Install Ubuntu 14.04.4 LTS (Trusty Tahr).

Open Terminal and execute following commands.

~~~
$ curl -sSL https://get.haskellstack.org/ | sh

$ sudo apt-get install libgtk2.0-dev libgtk-3-dev

$ git clone https://github.com/wkoiking/diagrams-gtk-template.git

$ cd diagrams-gtk-template

$ stack setup

$ stack build

$ stack exec sample-exe
~~~

## Windows


Check the Internet Connection.

### Installation of stack

Install from the following URL.

<https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows>

### Installation of MSYS2

Install from the following URL.

<http://msys2.github.io/>

※x86_64 for 64bit, i686 for 32bit

Execute following command from MSYS2 Shell:

~~~
$ pacman -Syu
...

警告: terminate MSYS2 without returning to shell and check for updates again
警告: for example close your terminal window instead of calling exit
(完了)
~~~

It will not return to prompt. So close the window manually.

Re open MSYS2 Shell and execute `pacman -Syu` again.

### Installation of pkg-config

Execute following command to install pkg-config from MSYS2 Shell:

(For 64bit OS)

~~~
$ pacman -S --force mingw-w64-x86_64-pkg-config
~~~

(For 32bit OS)

~~~
$ pacman -S --force mingw-w64-i686-pkg-config
~~~

### Installation of gtk2

Execute following command from MSYS2 Shell to install gtk2:

(For 64bit OS)

~~~
$ pacman -S --force mingw-w64-x86_64-gtk2
~~~

(For 32bit OS)

~~~
$ pacman -S --force mingw-w64-i686-gtk2
~~~

### Installation of Git for Windows

Install Git for Windows from following URL.

<https://git-for-windows.github.io/>

### Installation of diagrams-gtk-template

Clone diagrams-gtk-template repository by executing following command from git bash.

~~~
$ git clone https://github.com/wkoiking/diagrams-gtk-template.git
~~~
Open cmd.exe.
Change directory to diagrams-gtk-template repository created by above command.
Execute following commands:

~~~
$ stack install gtk2hs-buildtools

$ stack setup

$ cp /c/msys64/mingw64/bin/zlib1.dll /c/Users/UserName/AppData/Local/Programs/stack/x86_64-windows/ghc-8.0.2/mingw/bin/zlib1.dll

$ stack build

$ stack exec sample-exe
~~~

※In case of 32bit OS, please replace 64 to 32 in /diagrams-gtk-template/stack.cmd by text editor of your choice.

### Reference

<https://sites.google.com/site/toriaezuzakki/haskell/gtk2hs>

<https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows>
