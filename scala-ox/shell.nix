# From:
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/compilers/temurin-bin/jdk-linux-base.nix

{ pkgs ? import <nixpkgs> {}
, gtkSupport ? !pkgs.stdenv.targetPlatform.isGhcjs
}:

let
  runtimeDependencies = [
    pkgs.cups
  ] ++ pkgs.lib.optionals gtkSupport [
    pkgs.cairo pkgs.glib pkgs.gtk3
  ];
  runtimeLibraryPath = pkgs.lib.makeLibraryPath runtimeDependencies;

  result = pkgs.stdenv.mkDerivation {
    name = "openjdk-20-ea";

    src = pkgs.fetchurl {
      url    = "https://download.java.net/java/early_access/jdk20/35/GPL/openjdk-20-ea+35_linux-x64_bin.tar.gz";
      sha256 = "2062453caf72cff8ad296b84d90108f2eb057d7415a5c7d109672fd6b613ef1f";
    };

    buildInputs = [
      pkgs.alsa-lib # libasound.so wanted by lib/libjsound.so
      pkgs.fontconfig
      pkgs.freetype
      pkgs.stdenv.cc.cc.lib # libstdc++.so.6
      pkgs.xorg.libX11
      pkgs.xorg.libXext
      pkgs.xorg.libXi
      pkgs.xorg.libXrender
      pkgs.xorg.libXtst
      pkgs.zlib
    ] ++ pkgs.lib.optional pkgs.stdenv.isAarch32 pkgs.libffi;

    nativeBuildInputs = [ pkgs.autoPatchelfHook pkgs.makeWrapper ];

    # See: https://github.com/NixOS/patchelf/issues/10
    dontStrip = 1;

    installPhase = ''
      cd ..

      mv $sourceRoot $out

      # jni.h expects jni_md.h to be in the header search path.
      ln -s $out/include/linux/*_md.h $out/include/

      # Remove some broken manpages.
      # Only for 11 and earlier.
      [ -e "$out/man/ja" ] && rm -r $out/man/ja*

      # Remove embedded freetype to avoid problems like
      # https://github.com/NixOS/nixpkgs/issues/57733
      find "$out" -name 'libfreetype.so*' -delete

      # Propagate the setJavaClassPath setup hook from the JDK so that
      # any package that depends on the JDK has $CLASSPATH set up
      # properly.
      mkdir -p $out/nix-support
      printWords ${pkgs.setJavaClassPath} > $out/nix-support/propagated-build-inputs

      # Set JAVA_HOME automatically.
      cat <<EOF >> "$out/nix-support/setup-hook"
      if [ -z "\''${JAVA_HOME-}" ]; then export JAVA_HOME=$out; fi
      EOF

      # We cannot use -exec since wrapProgram is a function but not a command.
      #
      # jspawnhelper is executed from JVM, so it doesn't need to wrap it, and it
      # breaks building OpenJDK (#114495).
      for bin in $( find "$out" -executable -type f -not -name jspawnhelper ); do
        if patchelf --print-interpreter "$bin" &> /dev/null; then
          wrapProgram "$bin" --prefix LD_LIBRARY_PATH : "${runtimeLibraryPath}"
        fi
      done
    '';

    preFixup = ''
      find "$out" -name libfontmanager.so -exec \
        patchelf --add-needed libfontconfig.so {} \;
    '';

    # FIXME: use multiple outputs or return actual JRE package
    passthru = {
      jre = result;
      home = result;
    };
  };
in result
