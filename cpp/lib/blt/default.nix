{ pkgs ? (import <nixpkgs> {
    config.allowUnfree = true;
    config.segger-jlink.acceptLicense = true;
}), ... }:
pkgs.mkShell
{
	buildInputs = with pkgs; [
		cmake
		gcc
		clang
		emscripten
		ninja
		renderdoc
		valgrind
		gtest
		opentelemetry-cpp
		opentelemetry-cpp.dev
	];
	nativeBuildInputs = with pkgs; [
	    pkg-config
	    opentelemetry-cpp
        opentelemetry-cpp.dev
	];
	propagatedBuildInputs = with pkgs; [
		abseil-cpp
		protobuf
		grpc
		prometheus-cpp
		prometheus-cpp.dev
		openssl
		openssl.dev
		opentelemetry-cpp
		opentelemetry-cpp.dev
		civetweb
		civetweb.dev
		c-ares
		c-ares.dev
		nlohmann_json
		glibc
		glibc.dev
		curl
		libbacktrace
	];
	LD_LIBRARY_PATH="/run/opengl-driver/lib:/run/opengl-driver-32/lib";
}
