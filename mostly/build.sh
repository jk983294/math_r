# build from source
sudo apt install -y build-essential gfortran \
    libreadline-dev libx11-dev libxt-dev libpng-dev libjpeg-dev \
    libcairo2-dev libpcre2-dev libcurl4-openssl-dev \
    libssl-dev libxml2-dev zlib1g-dev libbz2-dev liblzma-dev \
    openjdk-17-jdk default-jdk
wget https://cran.r-project.org/src/base/R-4/R-4.3.2.tar.gz
tar xvf R-4.3.2.tar.gz
cd R-4.3.2
./configure --prefix=/usr/ --enable-R-shlib --with-x=yes --with-blas --with-lapack
make -j$(nproc)
sudo make install