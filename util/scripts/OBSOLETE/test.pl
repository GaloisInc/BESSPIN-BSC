#!/usr/bin/perl -w

use Gtk;         # load the Gtk-Perl module
use strict;      # a good idea for all non-trivial Perl scripts

init Gtk;        # initialize Gtk-Perl
set_locale Gtk;  # internationalize

# convenience variables for true and false
my $false = 0;
my $true = 1;

# widget creation
my $window = new Gtk::Window( "toplevel" );

{
    my $button1 = new Gtk::Button( "Goodbye World 1" );

    # callback registration
    $window->signal_connect( "delete_event", \&CloseAppWindow );   
    $button1->signal_connect( "clicked", \&CloseAppWindow );
    
    # show button
    $button1->show();
    
    # set window attributes and show it
    $window->add( $button1 );
}


$window->border_width( 15 );
$window->show();

# Gtk event loop
main Gtk;

# Should never get here
exit( 0 );



### Callback function to close the window
sub CloseAppWindow
  {
    Gtk->exit( 0 );
    return $false;
  }

