#!/usr/bin/perl
package SafeEval;

use Safe;

sub safe_eval {
    my($expr) = @_;
    my($cpt) = new Safe;
    #Basic variable IO and traversal
    $cpt->permit_only(qw(null scalar const padany lineseq leaveeval rv2sv));
    #Comparators
    $cpt->permit(qw(lt i_lt gt i_gt le i_le ge i_ge eq i_eq ne i_ne ncmp i_ncmp slt sgt sle sge seq sne scmp));
    #Base math
    $cpt->permit(qw(preinc i_preinc predec i_predec postinc i_postinc postdec i_postdec int hex oct abs pow multiply i_multiply divide i_divide modulo i_modulo add i_add subtract i_subtract));
    #Binary math
    $cpt->permit(qw(left_shift right_shift bit_and bit_xor bit_or negate i_negate not complement));
    #Regex
    $cpt->permit(qw(match split qr));
    #Conditionals
    $cpt->permit(qw(cond_expr flip flop andassign orassign and or xor));
    #Advanced math
    $cpt->permit(qw(atan2 sin cos exp log sqrt rand srand));
    my($ret) = $cpt->reval($expr);
    if ($@) {
	return undef;
    } else {
	return $ret;
    }
}

1;
