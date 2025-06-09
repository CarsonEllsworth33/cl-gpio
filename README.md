# cl-gpio
CFFI for the linux libgpiod library

## V2 api
The Rpi Bookworm repository at the time of this writing does not have the 2.X.X version of libgpiod which this binding references. However there is no issue in building the 2.X.X version from source.

To build from source download the following packages.
`sudo apt install autoconf`  
`sudo apt install autoconf-archive`  
`sudo apt install libtool`  

Git clone the repo  
`git clone https://github.com/brgl/libgpiod.git`  

Now build  
`cd libgpiod`  
`./autogen.sh`  
`./configure`  
`make`  
`sudo make install`  

Once the build is done it typically installs the library to /usr/local/lib.  
Run `sudo ln -s /usr/local/lib/libgpiod.so /usr/lib/libgpiod.so` to symlink the libgpiod.so lib to a location the the underlying CFFI library can find it.
