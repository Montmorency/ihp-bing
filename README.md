ihp-bing
=========
Prototype for an IHP bing maps plugin library.

How To
-------
The way it works is this:

    1) You acquire a Bing Map API key and store it in your IHP app configuration context.

```
        config :: ConfigBuilder
        config = do
            option Development
            option (AppHostname "localhost")
            option (bingAPIKey  "AWHOLENEWWORLD1998IHPISGREAT")
```
    
    2) ihp-bing defines some useful haskell bing data structures, FromJSON instances,
       and lenses to work with the Bing maps API.

    3) `Helper/Controller` and `Helper/View.hs` provide IHP style model controller and view
        rendering functions for the Bing data. 

Idea
------
nix should ensure a really nice dependency lock between the ihp-plugins and the current ihp release.

Would it be nice if there was a section of the IHP nix-expression that meant you could list your plugins?

```
    curry-house = [
                    ihp-stripe,
                    ihp-bing,
                    etc.
                  ]
```

And it either drops the plugin into the nix-store, or does a git checkout 
into your app-dev directory.

