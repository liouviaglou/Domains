#!/usr/bin/python

import pickle
import math
import pickle
import sys
import os

import os.path

accepted_chars = 'abcdefghijklmnopqrstuvwxyz '

model_data = pickle.load(open('/home/jupyter/local/Domains_202003/scripts/orig/gibb_detect/gib_model.pki', 'rb'))

# __location__ = os.path.realpath(
#     os.path.join(os.getcwd(), os.path.dirname(__file__)))
# model_data = pickle.load(open(os.path.join(os.getcwd(), 'gib_model.pki'), 'rb'))

pos = dict([(char, idx) for idx, char in enumerate(accepted_chars)])

def normalize(line):
    """ Return only the subset of chars from accepted_chars.
    This helps keep the  model relatively small by ignoring punctuation, 
    infrequenty symbols, etc. """
    return [c.lower() for c in line if c.lower() in accepted_chars]

def ngram(n, l):
    """ Return all n grams from l after normalizing """
    filtered = normalize(l)
    for start in range(0, len(filtered) - n + 1):
        yield ''.join(filtered[start:start + n])

def train():
    """ Write a simple model as a pickle file """
    k = len(accepted_chars)
    # Assume we have seen 10 of each character pair.  This acts as a kind of
    # prior or smoothing factor.  This way, if we see a character transition
    # live that we've never observed in the past, we won't assume the entire
    # string has 0 probability.
    counts = [[10 for i in xrange(k)] for i in xrange(k)]

    # Count transitions from big text file, taken 
    # from c
    # for line in open('/home/radmin/temp/py-code/big.txt'):
    for line in open('/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/orig/gibb_detect/big.txt'):
        for a, b in ngram(2, line):
            counts[pos[a]][pos[b]] += 1

    # Normalize the counts so that they become log probabilities.  
    # We use log probabilities rather than straight probabilities to avoid
    # numeric underflow issues with long texts.
    # This contains a justification:
    # http://squarecog.wordpress.com/2009/01/10/dealing-with-underflow-in-joint-probability-calculations/
    for i, row in enumerate(counts):
        s = float(sum(row))
        for j in xrange(len(row)):
            row[j] = math.log(row[j] / s)

    # Find the probability of generating a few arbitrarily choosen good and
    # bad phrases.
    # good_probs = [avg_transition_prob(l, counts) for l in open('/home/radmin/temp/py-code/good.txt')]
    # bad_probs = [avg_transition_prob(l, counts) for l in open('/home/radmin/temp/py-code/bad.txt')]
    good_probs = [avg_transition_prob(l, counts) for l  in open('/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/orig/gibb_detect/good.txt')]
    bad_probs = [avg_transition_prob(l, counts) for l in open('/Users/lubagloukhov/Documents/Consulting/Radix/Domains_202003/scripts/orig/gibb_detect/bad.txt')]

    # Assert that we actually are capable of detecting the junk.
    assert min(good_probs) > max(bad_probs)

    # And pick a threshold halfway between the worst good and best bad inputs.
    thresh = (min(good_probs) + max(bad_probs)) / 2
    pickle.dump({'mat': counts, 'thresh': thresh}, open('gib_model.pki', 'wb'))

def avg_transition_prob(l, log_prob_mat):
    """ Return the average transition prob from l through log_prob_mat. """
    log_prob = 0.0
    transition_ct = 0
    for a, b in ngram(2, l):
        log_prob += log_prob_mat[pos[a]][pos[b]]
        transition_ct += 1
    # The exponentiation translates from log probs to probs.
    return math.exp(log_prob / (transition_ct or 1))


def gibberish_test(l):
    model_mat = model_data['mat']
    threshold = model_data['thresh']
    return avg_transition_prob(l, model_mat)

    