#!/usr/bin/env perl
# MIME normalization boundary. No execution, rendering, or network access.
use strict;
use warnings;
use JSON::PP;
local $/;
my $message = decode_json(<STDIN>);
my $body = $message->{body};
if ($message->{bodySource} eq 'text/html') {
    require HTML::Parser;
    my $text = '';
    my %ignored;
    my $parser = HTML::Parser->new(api_version => 3);
    $parser->handler(start => sub {
        my ($tag) = @_;
        $ignored{$tag}++ if $tag =~ /^(script|style|head|template)$/;
        $text .= "\n" if !keys(%ignored) && $tag =~ /^(br|p|div|li|tr|h[1-6])$/;
    }, 'tagname');
    $parser->handler(end => sub {
        my ($tag) = @_;
        delete $ignored{$tag} if exists $ignored{$tag};
        $text .= "\n" if !keys(%ignored) && $tag =~ /^(p|div|li|td|tr|h[1-6])$/;
    }, 'tagname');
    $parser->handler(text => sub { $text .= $_[0] unless keys %ignored }, 'dtext');
    $parser->parse($body);
    $parser->eof;
    $body = $text;
}
$body =~ s/\x00//g;
$body =~ s/\s+/ /g;
$body =~ s/^ | $//g;
if ($body eq '') {
    $body = $message->{snippet} // '';
    $body =~ s/\x00//g;
    $message->{bodySource} = 'snippet';
}
$message->{truncated} = length($body) > 12000 ? JSON::PP::true : JSON::PP::false;
$message->{body} = substr($body, 0, 12000);
delete $message->{snippet};
print encode_json($message), "\n";
