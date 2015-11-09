#ASN.1 compiler plugin for Rebar3

Plugin for compiling ASN.1 definitions with Rebar3.

##Installation

Add plugin to your project `rebar.config` file.

    {plugins, [
        { rebar3_asn1_compiler, {git, "git@github.com:pyykkis/rebar3_asn1_compiler.git", {branch, "master"}}}
    ]}.

##Usage

Define ASN.1 compiler options in `rebar.config` and add output directory to Erlang source directories. Supported ASN.1 compiler options
are listed at http://www.erlang.org/doc/man/asn1ct.html. In addition, `{indir, "asn1"}` can be used for defining the directory containing
the ASN.1 modules. The default input directory is `"asn1"`.

    {asn1_opts, [{outdir, "asn1_gen"}, uper]}.
    {src_dirs, ["src", "asn1_gen"]}.

Additionally, plugin can be hooked into compile and clean commands.

    {provider_hooks, [
      {pre,
          [{clean,   {asn1, clean}},
           {compile, {asn1, compile}}]}
    ]}.

Stand-alone `compile` and `clean` commands are defined in `asn1` namespace

    rebar3 asn1 compile
    rebar3 asn1 clean

##Contributing

Feel free to send pull requests, feature requests or issue reports.

##License

MIT.
