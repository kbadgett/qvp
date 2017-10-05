## quick verilog parser : lightweight parser similar to rvp : http://www.burbleland.com/v2html/rvp/rvp.pod.html
## There is a need for design analysis, and having lightweight, simple parse tools is a necessity
## This package provides a basic verilog parser and should be easily extensible to create needed data structures
## author: kevin badgett
## Extend parser like this: add your own code to same ptable parse function
## package qvp;
## $ptable{module} = sub {$pstate = 'list_of_ports'; $MODULE = $token;
##                        $self->{modules}{$MODULE}{file} = $FILE;
##                        $THIS = $self->{modules}{$MODULE};
##                        print(STDERR " I see module $token\n");
##                      };
## package main;
package qvp;
_init();
=head1 read_verilog
reads in verilog files, parses them and stores results in an internal data structure (which I call a RVP database).
  Arguments:  - reference to array of files to read (can have paths)
              - reference to array of global includes - not used anymore,
                 just kept for backwards compatibility
              - reference to hash of defines with names as keys
              - quite flag. 1=be quiet, 0=be chatty.
              - reference to array of include directories
              - reference to array of library directories
              - library extension string (eg '.v') or reference to array of strings
  Returns:    - a pointer to the internal data structure.

  Example:
    $defines{'TRUE'}=1;  # same as +define+TRUE=1 on verilog cmd line
    $vdata = qvp->read_verilog(\@files,[],\%defines,1,\@inc_dirs,\@lib_dirs,\@lib_exts);

=cut
# pass 1: read all verilog files
# pass 2: resolve modules and instantiated parameters
sub read_verilog {
  my $class = ("ARRAY" eq ref $_[0]) ? "qvp" : shift;
  my ($VFILES, $global_includes, $DEFINE_REF, $local_quiet, $inc_dirs, $lib_dirs, $lib_ext_arg) = @_;

  $self = {};
  $self->{files}               = {}; # information on each file
  $self->{modules}             = {}; # pointers to module info in {files}
  $self->{defines}             = {};

  $errors = 0; $VERBOSE = ~$local_quiet;
  while( ($k,$v) = each(%{$DEFINE_REF})){ $self->{defines}{$k} = $v; }
  foreach my $dir (@{$inc_dirs}){ scan_dir($dir); }
  foreach my $dir (@{$lib_dirs}){ scan_dir($dir); }
  foreach my $vfile (@{$VFILES}){ parse_vfile($vfile,"",1); }
  foreach my $elab_group (keys %ELAB_CONNECTIONS){ # elaborate connection lists to port_lists
    my $port_num = 0;
    foreach my $e (@{$ELAB_CONNECTIONS{$elab_group}}){
      my($MODULE, $INST_MOD, $INST_NAME, $SIG, $line_num, $FILE) = @{$e};
      $port = $self->{modules}{$INST_MOD}{port_list}->[$port_num];
      $port = $port_num if(!defined $port);
      push(@{$self->{modules}{$INST_MOD}{sig}{$port}{con_to}}, {signal=>$SIG, module=>$MODULE, inst=>$INST_NAME} );
      push(@{$self->{modules}{$MODULE}{sig}{$SIG}{port_con}}, {module=>$INST_MOD, inst=>$INST, port=>$port, line=>$line_num, file=>$FILE} );
      $port_num++;
    }
  }
  bless($self,$class);
}
## -----------------------------------------------------------------------------
=head1 get_modules
Get a list of all the modules in the database.
  Returns:   - list of all the modules

  Example:   @all_modules = $vdata->get_modules();

=cut
sub get_modules {
  my ($self) = @_;
  if (wantarray) {
    return sort (keys %{$self->{modules}});
  } else { # in a scalar context keys returns the number of elements - sort doesn't
    return keys %{$self->{modules}};
  }
}
## -----------------------------------------------------------------------------
=head1 get_modules_parameters

Gets the parameters for a module.
  Arguments:  - module name
  Returns:    - A hash (well, really a list that can be assigned to a hash). 
               The keys of the hash are the parameters names. The values of the
               hash is everything (except comments) in the value.

  Example:    %parameters = $vdata->get_modules_parameters();
	      foreach my $p (keys %parameters) { ...

=cut
sub get_modules_parameters {
  my ($self,$module) = @_;
  my %r; my $ref = $self->{modules}{$module}{parameters};
  while(my ($p,$v) = each(%{$ref})){ $r{$p}=$v; }
  return %r;
}
## -----------------------------------------------------------------------------
=head1 get_files_full_name
Get the full name (including path) of a file.
  Arguments:  - name of file
  Returns:    - full path name

  Example  $full_name = $vdata->get_files_full_name($file);

=cut
sub get_files_full_name {
  my ($self,$file) = @_;
  return $self->{files}{$file}{full_name};
}
## -----------------------------------------------------------------------------
=head1 get_modules_file
Get the file name (no path) that a module is defined in.
  Arguments:  - module name
  Returns:    - file name without path, and the line number module starts on

  Example:    ($f) = $vdata->get_modules_file($m);

=cut
sub get_modules_file {
  my ($self,$module) = @_;
  return ($self->{modules}{$module}{file},0);
}
## -----------------------------------------------------------------------------
=head1 get_define
Find out where a define is defined and what the value is
  Arguments:  - name of the define
             Optional arguments where a you want the correct location and
               value for a particular use of a multiplely defined define:
              - file where define is used 
              - line where define is used
  Returns:    - list with three elements: file, line, value
                 or if the define does not exist it returns a empty list.
                 if the define was defined on the command line it sets file=""
                  and line=0

  Example:    ($f,$l,$v) = $vdata->get_define($word,$file,$line);

=cut
sub get_define {
  my ($self,$define,$file,$line) = @_;
  $define =~ s/^\`// ; # remove the ` if any
  if (!exists( $self->{defines}{$define} )){
    return ();
  } else {
    ($file, $line, $self->{defines}{$define}); ## file and line are inputs in rvp... just pass thru
  }
}
## -----------------------------------------------------------------------------
=head1 get_modules_signals

Get a list of all the signals in a particular module.
  Arguments:  - name of module
  Returns:    - list of signal names
  Example:    if ( @signs = $vdata->get_modules_signals($m))...

=cut
sub get_modules_signals {
  my ($self,$module) = @_;
  if (wantarray) {
    return sort (keys %{$self->{modules}{$module}{signals}});
  } else { # in a scalar context keys returns the number of elements - sort doesn't
    return keys %{$self->{modules}{$module}{signals}};
  }
}
sub get_modules_port_list {
  my ($self,$module) = @_;
  return @{$self->{modules}{$module}{port_list}};
}
## -----------------------------------------------------------------------------
=head1 get_module_signal
Get information about a particular signal in a particular module.
  Arguments:  - name of module
              - name of signal
  Returns:    - A list containing:
                 - the line signal is defined
                 - the line signal is assigned first (or -1)
                 - line in instantiating module where an input 
                       is driven from (or -1)
                 - the type of the signal (input,output,reg etc)
                 - the file the signal is in
                 - posedge flag (1 if signal ever seen with posedge)
                 - negedge flag (1 if signal ever seen with negedge)
                 - second type (eg reg for a registered output)
                 - signal real source file
                 - signal real source line
                 - range string if any ( not including [ and ] )
                 - the file signal is assigned first (or '')
                 - file for the instantiating module where an input 
                       is driven from (or "")
                 - a pointer to an array of dimensions for memories
                       each element of the array is a dimension, array
                       is empty for non-memories

  Note posedge and negedge information is propagated up the hierarchy to
  attached signals. It is not propagated down the hierarchy.

  Example:    ($s_line,$s_a_line,$s_i_line,$s_type,$s_file,$s_p,$s_n,
	       $s_type2,$s_r_file,$s_r_line,$range,$s_a_file,$s_i_file) = 
                      $vdata->get_module_signal($m,$sig);
=cut
sub get_module_signal {
  my ($self,$module,$sig) = @_;
  my $THIS   = $self->{modules}{$module};
  my $s_type = $THIS->{signals}{$sig}{type};
  my $range  = $THIS->{signals}{$sig}{range};
  ($s_line,$s_a_line,$s_i_line,$s_type,$s_file,$s_p,$s_n,$s_type2,$s_r_file,$s_r_line,$range,$s_a_file,$s_i_file);
}
sub get_module_signal_type {
  my ($self,$module,$sig) = @_;
  my $THIS   = $self->{modules}{$module};
  $THIS->{signals}{$sig}{type};
}
## -----------------------------------------------------------------------------
=head1 get_first_instantiator
Get the first thing that instantiates this module.
  Arguments:  - module name
  Returns:    - a 4 element list: instantiating module, file, instance name, line

  Example:
		($im,$f,$i) = $vdata->get_first_instantiator($m );
=cut
sub get_first_instantiator {
  my ($self,$module) = @_;
  if ( exists( $self->{modules}{$module} )) {
    $self->{current_instantiator}       =0;
    $self->{current_instantiator_module}=$module;
    return $self->get_next_instantiator();
  } else { return (); }
}
## -----------------------------------------------------------------------------
=head1 get_next_instantiator
Get the first thing that instantiates the module specified in 
get_first_instantiator (or _by_context).
  Returns:    - a 4 element list: instantiating module, file, instance name, line

  Example:
		($im,$f,$i) = $vdata->get_next_instantiator();
=cut
sub get_next_instantiator {
  my ($self) = @_;
  my ($module,$i) = ($self->{current_instantiator_module}, $self->{current_instantiator} );
  if (@{$self->{modules}{$module}{inst_by}} > $i ) {
    $self->{current_instantiator}++;
    return ($self->{modules}{$module}{inst_by}[$i]{module},
            $self->{modules}{$module}{inst_by}[$i]{file},
            $self->{modules}{$module}{inst_by}[$i]{inst},
            $self->{modules}{$module}{inst_by}[$i]{line});
  } else { return (); }
}
## -----------------------------------------------------------------------------
=head1 get_first_instantiation
Get the first thing that this module instantiates.
  Arguments:  - module name
  Returns:    - a 4 element list: instantiated module name, file, 
                  instance name, and line number
  Example:
		($im,$f,$i,$l) = $vdata->get_first_instantiation($m);
=cut
sub get_first_instantiation {
  my ($self,$module) = @_;
  if ( exists( $self->{modules}{$module} )) {
    $self->{current_instantiation}       =0;
    $self->{current_instantiation_module}=$module;
    return $self->get_next_instantiation();
  } else { return (); }
}
## -----------------------------------------------------------------------------
=head1 get_next_instantiation
Get the next thing that this module instantiates.
  Returns:    - a 4 element list: instantiated module name, file, 
                  instance name, and line number
  Example:
		($im,$f,$i,$l) = $vdata->get_next_instantiation();
=cut
sub get_next_instantiation {
  my ($self) = @_;
  my ($module,$i) = ($self->{current_instantiation_module}, $self->{current_instantiation} );
  if (@{$self->{modules}{$module}{instances}} > $i ){
    $self->{current_instantiation}++;
    return ($self->{modules}{$module}{instances}[$i]{module},
            $self->{modules}{$module}{instances}[$i]{file},
            $self->{modules}{$module}{instances}[$i]{inst},
            $self->{modules}{$module}{instances}[$i]{line});
  } else { return (); }
}
## -----------------------------------------------------------------------------
=head1 get_current_instantiations_parameters
Gets the parameters for the current instantiations (which is set using
get_first_instantiation and get_next_instantiation).  If the
instantiation parameters does not use the verilog 2001 .name(...)
syntax and rvp does not have the access to the source of the module
then the parameter names will be returned as numbers reflecting the
order (starting at 0).
  Returns:    - A hash (well, really a list that can be assigned to a hash). 
               The keys of the hash are the parameters names. The values of the
               hash is everything (except comments) in the value.

  Example:    %parameters = $vdata->get_current_instantiations_parameters();
	      foreach my $p (keys %parameters) { ...
=cut
sub get_current_instantiations_parameters {
  my ($self) = @_;
  my ($module,$i) = ($self->{current_instantiation_module}, ($self->{current_instantiation} - 1) );
  my %r; warn("Need to start iterator with call to get_first_instantiation\n") if($i < 0);
  if (@{$self->{modules}{$module}{instances}} > $i ){
    my $pref = $self->{modules}{$module}{instances}[$i]{parameters};
    if( $pref ){ foreach my $p (keys %{$pref}) { $r{$p}=$pref->{$p}; } }
  }
  return %r;
}
## -----------------------------------------------------------------------------
=head1 get_first_signal_con_to
Get the first signal that is connected to this port in an
instantiation of this module. This only works for instances that use
the .port(sig) notation.
  Arguments:  - module name
              - signal name
  Returns:    - a 4 element list: signal connected to this port
                                  module signal is in
		                  instance (of this module) where the connection
			            occurs

  Example:    ($cts,$ctm,$cti)=$vdata->get_first_signal_con_to($m,$s);
=cut
sub get_first_signal_con_to{
    my ($self,$module,$signal) = @_;

    $self->{current_signal_con_to}       =0;
    $self->{current_signal_con_to_module}=$module;
    $self->{current_signal_con_to_module_signal}=$signal;
    return $self->get_next_signal_con_to();
}
## -----------------------------------------------------------------------------
=head1 get_next_signal_con_to
Get the next signal that is connected to this port in an
instantiation of this module. This only works for instances that use
the .port(sig) notation.
  Arguments:  - module name
              - signal name
  Returns:    - a 4 element list: signal connected to this port
                                  module signal is in
		                  instance (of this module) where the connection occurs
  Example:    ($cts,$ctm,$cti)=$vdata->get_next_signal_con_to();
=cut
sub get_next_signal_con_to{
  my ($self) = @_;
  my ($module,$signal,$i,$ctref) = ($self->{current_signal_con_to_module},
                                    $self->{current_signal_con_to_module_signal}, $self->{current_signal_con_to}, 0);
  $ctref = $self->{modules}{$module}{sig}{$signal}{con_to};
  if (@{$ctref} > $i ) {
    $self->{current_signal_con_to}++;
    return ( $ctref->[$i]{signal},$ctref->[$i]{module},$ctref->[$i]{inst});
  } else { return (); }
}
## -----------------------------------------------------------------------------
=head1 get_first_signal_port_con
Get the first port that this signal in this module is connected to.
  Arguments:  - module name
              - signal name
  Returns:    - a 5 element list: instantiated module name, instance name
                  port name, line number and file
  Example:    ($im,$in,$p,$l,$f)=$vdata->get_first_signal_port_con($m,$s);
=cut
sub get_first_signal_port_con{
    my ($self,$module,$signal) = @_;

    $self->{current_signal_port_con}       =0;
    $self->{current_signal_port_con_module}=$module;
    $self->{current_signal_port_con_module_signal}=$signal;
    return $self->get_next_signal_port_con();
}
## -----------------------------------------------------------------------------
=head1 get_next_signal_port_con
Get the next port that this signal in this module is connected to.
  Returns:    - a 5 element list: instantiated module name, instance name
                  port name, line number and file
  Example:    ($im,$in,$p,$l,$f)=$vdata->get_next_signal_port_con();
=cut
sub get_next_signal_port_con{
    my ($self) = @_;
    my ($module,$signal,$i,$pcref);

    $module = $self->{current_signal_port_con_module};
    $signal = $self->{current_signal_port_con_module_signal};
    $i      = $self->{current_signal_port_con};

    $pcref = $self->{modules}{$module}{sig}{$signal}{port_con};
    if (@{$pcref} > $i ) {
	$self->{current_signal_port_con}++;
	return ( $pcref->[$i]{module},$pcref->[$i]{inst},$pcref->[$i]{port},$pcref->[$i]{line},$pcref->[$i]{file});
    }
    else {
	return ();
    }
}
### -----------------------------------------------------------------------------
### -----------------------------------------------------------------------------
sub scan_dir { # cache file location for easy lookup
  push(@scan_dirs, $_[0]);
  while(my $dir = shift(@scan_dirs)){ ## recursive traversal
    opendir(D,$dir);
    foreach (readdir(D)){
      next if(/^\./);         # no dot files
      next if($_scanned{$_}); # prevent loops
      my $f = "$dir/$_"; if(-d $f){ $_scanned{$_} = 1; push(@scan_dirs,$f) } else { $FIND_FILE{$_} = $dir; }
    }
    closedir(D);
  }
}
### -----------------------------------------------------------------------------
## recursive verilog parser:
#    handle preprocessing & tokenizing (The lexer), then call pstate functions to parse tokens
sub parse_vfile {
  my ($f,$rd, $regular_file) = @_;
  return if($ALREADY_PARSED{$_[0]}); $ALREADY_PARSED{$_[0]} = $regular_file;
  ## print("DEBUG: parse_vfile $_[0]\n");
  local (*V, $line_num, $L, $FILE, $rel_dir);          # stacked varables with which we will resume parsing.
  if($f =~ /^(\S*\/)([^\/]+)$/){
    ($rel_dir, $FILE) = ("$rd$1",$2) } # $FILE is the base file for which we are looking
  else {
    ($rel_dir, $FILE) = ($rd,$f);
  }
  if(!open(V,$f)){
    if(!open(V,"$rel_dir$FILE")){
      $f = ($dir = $FIND_FILE{$FILE})? "$dir/$FILE" : "./$FILE";
      if(!open(V, $f)){ print("Cannot open file $f !!!\n"); return(); }
    }
  }
  $self->{files}{$FILE}{full_name} = $f;
  $line_num = 0; $file_lvl++; if(1 == $file_lvl){ parse_init(); }
  while($L = <V>){
    $line_num++;      ## print("DEBUG ($PP):$line_num: $pstate : $L");
    if($commented_out){ if($L=~/\*\//g){ $commented_out = 0; $L = substr($L, pos($L));} else { next; } }
    if($ppcontinue){ $ppcontinue = 0; } else { $ppstate = 0; }
    $ppcontinue = 0;
    while($L=~/([ \n\t]+|\/\*|\/\/|;|,|:|\.|\(|\)|\[|\]|@|\#|\{|\}|\/|[!<=>]+|"|->|[^;:\.\s\(\)\{\}\@\#\[\]\/,!<=>"]+)/g ){
      $token = $1;
      if($ppstring){ $STRING .= $token; if($token eq '"'){ $ppstring = 0; $token = $STRING; } else { next; } }
      elsif($token eq '"'){ $ppstring = 1; $STRING = '"'; next; }
      if($token =~ /^[ \t\n]/){ next; } ## skip white space
      if($token eq '//'){ last; }
      if($token eq '/*'){ $commented_out = 1; my $p1 = pos($L);
			  if($L=~/\*\//g){ $commented_out = 0; $L = substr($L, pos($L)); next; } else { last; }
			}
      if(0 == $PP){ if(   $token =~ /`ifdef|`ifndef/){ unshift(@PP,0); }
                    elsif($token eq '`else' ){ $PP = $PP[0]; }
                    elsif($token eq '`endif'){ $PP = shift(@PP); }
                    next; }
      if($ppstate > 0 ){ if($token eq "\\"){ $ppcontinue = 1; }
                         elsif(1  == $ppstate){ $D = $token; $ppstate = 2; $self->{defines}{$D} = '';} ## `define
                         elsif(2  == $ppstate){ $self->{defines}{$D} .= $token; }
                         elsif(4  == $ppstate){ $timescale .= $token; }
                         elsif(20 == $ppstate){ unshift(@PP,$PP); $PP = (defined($self->{defines}{$token}))? 1 : 0; $ppstate = 0;}
                         elsif(21 == $ppstate){ unshift(@PP,$PP); $PP = (defined($self->{defines}{$token}))? 0 : 1; $ppstate = 0;}
                         elsif(5  == $ppstate){ $ppstate = 0; my $file = $token; $file =~ s/"//g; $L = substr($L, pos($L)); parse_vfile($file, $rel_dir, 0); } ## `include
                         next;
                       }
      if(defined(my $f = $PREPROC{$token})){ $f->(); next; }
      ## print("DEBUG $pstate: $token\n");
      $ptable{$pstate}->();
    }
  }
  close(V); $file_lvl--;
  if((0 == $file_lvl) && ($pstate ne '0') ){
    print(STDOUT "ERROR: Final pstate was [$pstate] expected 0.\nParser / Syntax error: file: $self->{files}{$FILE}{full_name}\n"); $errors++;
  }
}
### -----------------------------------------------------------------------------
sub parse_init {
  $pstate    = 0; # parse state
  $ppstate   = 0; # pre-processor state
  $ppstring  = 0; # gather string token
  $PP        = 1; # define state (evaluated `define state for code inclusion)
  $commented_out = 0;
  @rstate    = ('error');# clear the return state stack.
  $paren_lvl = 0; $param_lvl = 0; @if = ();
}
sub add_parameter { $THIS->{parameters}{$PARAM} = $V; push(@{$THIS->{param_list}}, $PARAM); if($localparam){ $THIS->{localparam}{$PARAM} = $V; } }
sub _init {
  $file_lvl  = 0;
  $PREPROC{'`include'}= sub { $ppstate = 5;  };
  $PREPROC{'`define'} = sub { $ppstate = 1;  };
  $PREPROC{'`ifdef'}  = sub { $ppstate = 20; };
  $PREPROC{'`ifndef'} = sub { $ppstate = 21; };
  $PREPROC{'`else'}   = sub { $PP = ($PP)? 0 : 1; };
  $PREPROC{'`endif'}  = sub { $PP = shift(@PP); $ifdef_lvl--; };
  $PREPROC{'`timescale'} = sub { $ppstate = 4; $timescale = ''; };
  foreach (qw(input inout output)){ $STDIO{$_} = 1; }
  foreach (qw(wire reg input inout output integer wor wand logic)){ $T_DECLARE{$_} = 1; }
  foreach (qw(case casez casex)){ $T_CASE{$_} = 1; }
  foreach (qw(always always_comb and assign attribute begin buf bufif0 bufif1 case casex casez cmos deassign default
              defparam disable edge else end endattribute endcase endfunction endmodule endprimitive
              endspecify endtable endtask event for force forever fork function highz0 highz1 if ifnone
              initial inout input integer join medium module large macromodule nand negedge nmos nor not
              notif0 notif1 or output parameter localparam pmos posedge primitive pull0 pull1 pulldown pullup rcmos
              real realtime reg release repeat rnmos rpmos rtran rtranif0 rtranif1 scalared signed small
              specify specparam strength strong0 strong1 supply0 supply1 table task time tran tranif0 tranif1
              tri tri0 tri1 triand trior trireg unsigned vectored wait wand weak0 weak1 while wire
              wor xnor xor ; = < > @ ( ) [ ] import export sequence)){ $KEYWORD{$_} = sub { }; }
  sub temp_block { push(@rstate, $pstate); $pstate = 'temp_block'; };
  $KEYWORD{endmodule}  = sub { $pstate = 0; };
  $KEYWORD{parameter}  = sub { push(@rstate, $pstate); $localparam = 0; $pstate = 'parameter_dec'; $V = ''; $assign = 0; };
  $KEYWORD{localparam} = sub { push(@rstate, $pstate); $localparam = 1; $pstate = 'parameter_dec'; $V = ''; $assign = 0; };
  $KEYWORD{assign} = sub { push(@rstate, $pstate); $pstate = 'assign'; };
  $KEYWORD{case}   = sub { push(@rstate,($pstate, 'case')); $pstate = 'expression'; $paren_lvl = 0; };
  $KEYWORD{casez}  = sub { push(@rstate,($pstate, 'case')); $pstate = 'expression'; $paren_lvl = 0; };
  $KEYWORD{casex}  = sub { push(@rstate,($pstate, 'case')); $pstate = 'expression'; $paren_lvl = 0; };
  $KEYWORD{if}     = sub { push(@rstate,($pstate, 'if_begin'));  $pstate = 'expression'; $paren_lvl = 0; };
  $KEYWORD{repeat} = sub { push(@rstate,($pstate, 'statement')); $pstate = 'expression'; $paren_lvl = 0; };
  $KEYWORD{initial} = sub{ push(@rstate, $pstate); $pstate = 'statement'; };
  $KEYWORD{while}  = sub { push(@rstate,($pstate, 'statement')); $pstate = 'expression'; $paren_lvl = 0; };
  $KEYWORD{fork}   = sub { push(@rstate, $pstate); $pstate = 'fork'; };
  $KEYWORD{for}    = sub { push(@rstate,($pstate, 'statement')); $pstate = 'for_expr'; };
  $KEYWORD{always}      = sub { push(@rstate, ($pstate, 'statement')); $pstate = 'expression'; $paren_lvl = 0; };
  $KEYWORD{always_comb} = sub { push(@rstate, ($pstate, 'statement')); $pstate = 'expression'; $paren_lvl = 0; };
  $KEYWORD{'->'}       = \&temp_block;
  $KEYWORD{'disable'}  = \&temp_block;
  $KEYWORD{'deassign'} = \&temp_block;
  $KEYWORD{'force'}    = \&temp_block;
  $KEYWORD{'release'}  = \&temp_block;
  $KEYWORD{'function'} = sub { push(@rstate, $pstate); $pstate = 'function'; };
  $KEYWORD{'task'}     = sub { push(@rstate, $pstate); $pstate = 'task'; };
  $KEYWORD{'genvar'}   = sub { push(@rstate, $pstate); $pstate = 'genvar'; };
  $KEYWORD{'generate'} = sub { push(@rstate, $pstate); $pstate = 'generate_block'; };
  $KEYWORD{'property'} = sub { push(@rstate, $pstate); $pstate = 'property'; };
  $KEYWORD{'sequence'} = sub { push(@rstate, $pstate); $pstate = 'sequence'; };
  $KEYWORD{'import'}   = \&temp_block;
  $KEYWORD{'export'}   = \&temp_block;
  $KEYWORD{'void'}     = sub { push(@rstate, $pstate); $pstate = 'sv_function'; $return_type = $token; };
  $KEYWORD{'int'}      = sub { push(@rstate, $pstate); $pstate = 'sv_function'; $return_type = $token; };
  $KEYWORD{covergroup} = sub { push(@rstate, $pstate); $pstate = 'covergroup'; };
  ## ----------------------------------------------------------------------------
  # to eliminate many internal states, we parse a superset of the language and let compilers parse precise syntax
  # our goal is to create a parser that simplifies creation of data structures for design analysis
  %ptable = (0          => sub {$pstate = 'module' if($token eq 'module' ); },
             module     => sub {$pstate = 'list_of_ports'; $MODULE = $token;
                                $self->{modules}{$MODULE}{file} = $FILE;
                                $THIS = $self->{modules}{$MODULE};
                              },
             list_of_ports => sub { if   ($token =~ /\(|,|\)/){ }
                                    elsif(defined $STDIO{$token} ){ $SIG_TYPE = $token; $RANGE = ''; }
                                    elsif($token eq 'reg' ){ $SIG_TYPE .= " $token"; $RANGE = ''; }
                                    elsif($token eq 'wire'){ $SIG_TYPE .= " $token"; $RANGE = ''; }
                                    elsif($token eq '#'){push(@rstate, $pstate); $pstate = 'parameter_dec'; $param_lvl = 0; $assign = 0; }
                                    elsif($token eq ';'){$pstate = 'module_item'; }
                                    elsif($token eq '['){push(@rstate, $pstate); $pstate = 'range'; }
                                    else {
                                      push(@{$THIS->{port_list}}, $token );
                                      $THIS->{signals}{$token}{type}  = $SIG_TYPE;
                                      $THIS->{signals}{$token}{range} = $RANGE;
                                    }
                                  },
             ## --------------------
             range => sub { if(   $token eq ']'){$pstate = pop @rstate; } ## return to caller state
                            else { $RANGE .= "$token "; }
                          },
             parameter_dec => sub { if(   $token =~ /parameter/){ $assign = 0; }
                                    elsif($token eq '='){ $assign = 1; $V = ''; }
                                    elsif($token eq ')'){ ## v2k module_parameter before port_list
                                      $V .= $token; $param_lvl--; if($param_lvl == 0){ add_parameter(); $pstate = pop @rstate; } }
                                    elsif($token eq ','){ add_parameter(); $assign = 0; }
                                    elsif($token eq '('){ $V .= $token; $param_lvl++; }
                                    elsif($token eq ';'){ add_parameter(); $pstate = pop @rstate; } ## v95 normal parameter
                                    else { if($assign){ $V .= $token; } else { $PARAM = $token; } }
                                  },
             param_assign => sub { if(   $token eq ')'){ $pstate = pop @rstate; }
                                   elsif($token eq '.'){ $pstate = 'param_assign_named1'; }
                                   elsif($token eq '('){ } # opening paren
                                   elsif($token eq ','){ }
                                   else {$P = shift @plist; $param->{$P} = $token; }
                                 },
             param_assign_named1 => sub { if($token eq '('){ $param_lvl++; $pstate = 'param_assign_named2'; } else { $P = $token; $V = ''; } },
             param_assign_named2 => sub {
               if(   $token eq ')'){ ## special case to not include enclosing parenthesis around named parameter values.
                 if(1 == $param_lvl){ $pstate = 'param_assign'; $param->{$P} = $V; $param_lvl = 0; } else { $V .= $token; $param_lvl--; } }
               else { $V .= $token; if($token eq '('){$param_lvl++; } }
             },
             ## --------------------
             module_item => sub { if(   $T_DECLARE{$token}){ $pstate = 'signal_dec'; $SIG_TYPE = $token; $RANGE = ''; }
                                  elsif(defined($f = $KEYWORD{$token})){ $f->(); }
                                  else{ $param = ''; $pstate = 'module_inst1'; $INST_MOD = $token; $got_param = 0; }
                                },
             ## --------------------
             signal_dec => sub { if(   $token eq ';'){$pstate = 'module_item';
                                                      $THIS->{signals}{$SIG}{type}  = $SIG_TYPE;
                                                      $THIS->{signals}{$SIG}{range} = $RANGE;
                                                    }
                                 elsif($token eq '['){ push(@rstate, $pstate); $pstate = 'range'; }
                                 elsif($token eq ','){$THIS->{signals}{$SIG}{type}  = $SIG_TYPE;
                                                      $THIS->{signals}{$SIG}{range} = $RANGE; }
                                 else {$SIG = $token; }
                               },
             module_inst1 => sub { if(   $token eq '#'){ push(@rstate, $pstate); $param = {}; $pstate = 'param_assign'; @plist = @{$THIS->{param_list}}; }
                                   elsif($token eq ':'){ $pstate = 'property_assert'; }
                                   else { $pstate = 'module_inst2'; $INST_NAME = $token; }
                                 },
             module_instx => sub {if($token eq ']'){ $pstate = 'module_inst2'; } $INST_NAME .= $token; },
             module_inst2 => sub {
               if(   $token eq '['){$pstate = 'module_instx'; $INST_NAME .= $token; }
               elsif($token eq '('){$pstate = 'module_inst3'; $paren_lvl = 1; $elab_group++; ## print("DEBUG $FILE ($line_num): $MODULE $INST_MOD $INST_NAME $param\n"); 
                                    push(@{$self->{modules}{$INST_MOD}{inst_by}},   {module=>$MODULE,   inst=>$INST_NAME, file=>$FILE, line=>$line_num});
                                    push(@{$THIS->{instances}}, {parameters=>$param, module=>$INST_MOD, inst=>$INST_NAME, file=>$FILE, line=>$line_num});
                                  } else { $pstate = 'module_item'; } },
             module_inst3 => sub { if(   $token eq '.'){ $pstate = 'port_con'; }
                                   elsif($token eq ')'){ $paren_lvl--; }
                                   elsif($token eq ','){ if(0 == $paren_lvl){ $pstate = 'module_inst1'; } }
                                   elsif($token eq ';'){ $pstate = 'module_item'; }
                                   elsif($token eq '('){ $pstate = 'port_con_list'; $paren_lvl++; $SIG = $token; }
                                   elsif($token eq '{'){ $pstate = 'port_con_list'; $paren_lvl++; $SIG = $token; } # concat is like paren
                                   else { $SIG = $token; $pstate = 'port_con_list'; }
                                 },
             port_con_list => sub {
               if(    $token eq ')' ){ $paren_lvl--; }
               elsif( $token eq '}' ){ $paren_lvl--; }
               elsif( $token eq '(' ){ $paren_lvl++; }
               elsif( $token eq '{' ){ $paren_lvl++; }
               elsif( $token eq '.' ){ $pstate = 'port_con'; }
               if($paren_lvl <= 1){
                 if( $token =~ /,|\)/ ){ $pstate = 'module_inst3'; push(@{$ELAB_CONNECTIONS{$elab_group}}, [$MODULE, $INST_MOD, $INST_NAME, $SIG, $line_num, $FILE] ); }
               } else {
                 $SIG .= " $token";
               }
             },
             port_con     => sub { if($token eq '('){ $pstate = 'port_connected'; $SIG = ''; $paren_lvl++; } else { $port = $token; } },
             port_connected=>sub {
               if($token eq ')'){
                 $paren_lvl--;
                 if($paren_lvl == 1){
                   $pstate = 'module_inst3';
                   push(@{$self->{modules}{$INST_MOD}{sig}{$port}{con_to}}, {signal=>$SIG, module=>$MODULE, inst=>$INST_NAME} );
                   push(@{$self->{modules}{$MODULE}{sig}{$SIG}{port_con}}, {module=>$INST_MOD, inst=>$INST, port=>$port, line=>$line_num, file=>$FILE} );
                 } else { $SIG .= $token; }
               } else { if($token eq '('){ $paren_lvl++; } $SIG .= $token; }
             },
             ## --------------------
             statement => sub { if($token eq 'begin'){ $pstate = 'statements'; }
                                else { $pstate = pop @rstate;
                                       if(defined($f = $KEYWORD{$token})){ $f->(); } # statement keywords
                                       else { push(@rstate, $pstate); $pstate = 'block_assigns'; $LVALUE = $token; }
                                     }
                              },
             statements => sub { if($token eq 'end'){ $pstate = pop @rstate; }
                                 elsif(defined($f = $KEYWORD{$token})){ $f->(); } # statement keywords
                               },
             block_assigns => sub { if($token eq ';'){ $pstate = pop @rstate; } }, ## or task/function calls
             ## --------------------
             if_begin      => sub { if($token eq 'else'){ push(@rstate, 'if_else'); return; }
                                    else { push(@rstate, 'if'); }
                                    $ptable{statement}->();
                                  },
             if            => sub { if($token eq 'else'){ push(@rstate, 'if_else'); return; }
                                    else { $pstate = pop @rstate; }
                                    $ptable{$pstate}->();
                                  },
             if_else       => sub { $ptable{statement}->(); },
             ## --------------------
             expression => sub { if   ($token eq ')'      ){ $paren_lvl--; if(0 == $paren_lvl){ $pstate = pop @rstate; } }
                                 elsif($token eq '*'      ){ if(0 == $paren_lvl){ $pstate = pop @rstate; } }
                                 elsif($token eq '('      ){ $paren_lvl++; }
                                 elsif($token eq 'begin'  ){ if(0 == $paren_lvl){ $pstate = pop @rstate;
                                                                                  if($pstate eq 'statement'){ $pstate = 'statements'; } } }
                               },
             assign     => sub { if($token eq ';'      ){ $pstate = pop @rstate; } },
             case       => sub { if   ($token eq 'endcase'){ $pstate = pop @rstate; }
                                 elsif($T_CASE{$token} ){ push(@rstate,($pstate, 'case')); $pstate = 'expression'; $paren_lvl = 0; };
                               },
             for_expr   => sub { if($token eq ')'      ){ $pstate = pop @rstate; } }, ## BOZO: do something with iterator variables
             fork       => sub { if($token =~ /join|join_any|join_none/ ){ $pstate = pop @rstate; } },
             function   => sub { if($token eq 'endfunction'){ $pstate = "named_block1"; } },
             task       => sub { if($token eq 'endtask'    ){ $pstate = "named_block1"; } },
             temp_block => sub { if($token eq ';'      ){ $pstate = pop @rstate; } },
             ## --------------------
             genvar     => sub { if($token eq ';'      ){ $pstate = pop @rstate; }
                                 elsif($token eq ','   ){ }
                                 else { $GENVAR{$token} = 1; }
                               },
             generate_block => sub { if($token eq 'endgenerate'){ $pstate = pop @rstate; } },
             sequence       => sub { if($token eq 'endsequence'){ $pstate = 'named_block1' } },
             sv_function    => sub { if($token eq ';'          ){ $pstate = pop @rstate; } },
             property       => sub { if($token eq 'endproperty'){ $pstate = 'named_block1'; } },
             named_block1   => sub { if($token eq ':'          ){ $pstate = 'named_block2'; } else { $pstate = pop @rstate; $ptable{$pstate}->(); } },
             named_block2   => sub { $pstate = pop @rstate; },
             property_assert=> sub { if(   $token eq 'begin'){ $pstate = 'propassert_block'; }
                                     elsif($token eq ';'    ){ $pstate = 'module_item'; } },
             propassert_block=>sub { if(   $token eq 'end'  ){ $pstate = 'module_item'; } }, # re-examine assert statements.
             covergroup     => sub { $KEYWORD{$token} = \&temp_block;  $pstate = 'covergroup1'; },
             covergroup1    => sub { if(   $token eq 'endgroup'){ $pstate = pop @rstate; } },
             ## --------------------
             error          => sub { print(" SYNTAX error: line $line_num \n"); },
            );
}
### -----------------------------------------------------------------------------
