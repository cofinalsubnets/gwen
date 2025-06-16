# Maintainer: gwem <cofinalsubnets@gmail.com>
pkgname=gwen-git
pkgver=0.0.0
pkgrel=1
pkgdesc="list expression interpreter"
arch=(x86_64 i686 aarch64 armv7h)
url="https://github.com/cofinalsubnets/gwen"
license=(Unlicense)
depends=('glibc')
makedepends=('git')
provides=('gwen')
_gitname="${pkgname}"
source=("${_gitname}::git+https://github.com/cofinalsubnets/gwen.git")
sha256sums=(SKIP)

pkgver() {
  cd ${_gitname}
  git checkout main &> /dev/null
  git describe --always | sed 's/-/_/g'
}
  
build() {
  cd ${_gitname}
  git checkout main &> /dev/null
  make all
}

check() {
  cd ${_gitname}
  git checkout main &> /dev/null
  make test
}

package() {
  cd ${_gitname}
  git checkout main &> /dev/null
  make DESTDIR="${pkgdir}/" PREFIX="usr/" VIMPREFIX="usr/share/vim/vimfiles/" install
}
