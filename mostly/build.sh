# build from source
apt install -y libcairo2-dev libpango1.0-dev libjpeg-dev libgif-dev librsvg2-dev
sudo apt install -y build-essential gfortran \
    libreadline-dev libx11-dev libxt-dev libpng-dev libjpeg-dev \
    libcairo2-dev libpcre2-dev libcurl4-openssl-dev \
    libssl-dev libxml2-dev zlib1g-dev libbz2-dev liblzma-dev \
    openjdk-17-jdk default-jdk xorg-dev x11proto-core-dev \
    libgif-dev libtiff5-dev
wget https://cran.r-project.org/src/base/R-4/R-4.3.2.tar.gz
tar xvf R-4.3.2.tar.gz
cd R-4.3.2
./configure \
  --prefix=/usr/ \
  --enable-R-shlib \
  --with-x=yes \
  --x-includes=/usr/include/X11 \
  --x-libraries=/usr/lib/x86_64-linux-gnu \
  --with-cairo \
  --with-jpeglib \
  --with-readline \
  --with-blas \
  --with-lapack
make -j$(nproc)
sudo make install