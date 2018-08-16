# Installing Xmonad on Debian 9  with stack 1.6.3 and ghc-8.2.2 #

## Clone the following repositories inside ~/.xmonad/##

- git clone "https://github.com/xmonad/xmonad" xmonad-git
- git clone "https://github.com/xmonad/xmonad-contrib" xmonad-contrib-git
- git clone "https://github.com/jaor/xmobar" xmobar-git

## Generate stack configuration file ##

- stack init
- add `iwlib-0.1.0` to the `extra-deps:`
- add `xmobar all_extensions: true` to flags 

```yaml

flags:
  xmobar:
    all_extensions: true

```

```yaml
    extra-deps:
        - iwlib-0.1.0
```

## Install libxml2  ##

- sudo apt-get install libxml2-dev -y

## Build Xmonad, Xmobar and Xmonad-contrib ##

- stack install


## Add custom build script ##

- `$HOME/.xmonad/build` and make sur it's executable

```bash
#!/bin/sh

exec stack ghc --  --make xmonad.hs -i -ilib -fforce-recomp -main-is main -v0  -o "$1"

```

## Copy xmonad.desktop ##

```bash
 
 sudo cp xmonad.desktop /usr/share/xsessions/

```
