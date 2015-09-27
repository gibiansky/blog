I’ve recently started working at Karius, working on the software platform
behind a new way to test for infectious disease. We’ve started growing quickly
recently, and so we’ve been thinking a lot about how you can set up a culture
of high quality engineering. What can you do to ensure that as you grow, you
stay focused on high-quality code, architecture, and product? This blog post
describes some of the conclusions we’ve come to in our discussions. Some of
these ideas may be applicable to any software business; some of them should be
obvious to anyone who’s worked in a culture of high-quality engineering; others
are specific to what we care about at Karius and may not make sense everywhere.
These are simply our thoughts on good engineering practices, and take from this
what you will. This post is written as a set of suggestions, with some of the
language assuming you’re not already doing these things – if you are, that’s
great, but if you’re not, this post provides ways to gradually ramp up and ease
in to high-quality engineering practices.

As an aside: if you’re interested in working on a team of crazy biologists,
engineers, data scientists, and clinicians trying to forever change the way
infectious diseases are diagnosed and treated, we’re hiring anyone who wants to
use software to push the limits of diagnostic and genomic technologies, with a
focus on software engineering, infrastructure, devops, frontend and backend
development, computational biology, and machine learning. Take a look at our
[AngelList job postings](https://angel.co/karius/jobs) or send me an email! (My
email is in the sidebar.)

# Why “Culture”?
Good engineering isn’t just something done by one person. As with any
collaborative enterprise, it requires team buy-in, discipline, and vigilance to
create an amazing engineering initiative. It’s not just about doing the right
thing yourself – it’s also about making it *easy* to do things the right way
and *hard* to do things the wrong way. It’s human nature to try to take
shortcuts, make excuses for “just this time”, and so on, and to avoid letting
ourselves make your lives more difficult in the future, you can set up a
culture that *encourages* good engineering from the start.

# Code Reviews
Code reviews are an incredibly powerful tool for creating a high-quality codebase.

## Reasons:
- **Knowledge Redundancy:** Having at least one other set of eyes go over all
  code committed to your codebases ensures that at least two people in the
  company know how all code works. This avoids the situation where only one
  person knows how a codebase works, and they go on vacation or leave the
  company, and suddenly we’re left with a codebase no one can really work with,
  or you have to call up someone on their vacation with an emergency
  maintenance request.
- **Maintainability:** Code is written not only for computers to understand,
  but also for future readers and maintainers to understand. By forcing at
  least one other person to go over it and understand it, you ensure that the
  code that’s written can be read and understood later. If the code is hard to
  understand for any reason, the code reviewer must identify that and make sure
  it is rewritten to be understandable (reorganized, commented, made more
  modular, etc).
- **Detecting Bugs:** Just like spelling errors can be incredibly hard to catch
  when you’re reading your own writing, bugs can be incredibly difficult to
  catch when you’re reading your own code. You know what the code is intended
  to do, so you can miss places where it actually does something different,
  especially in edge cases you may not have envisioned. An external reviewer
  who doesn’t *quite* understand the intention of the code will scrutinize it
  much more thoroughly, catching small but important bugs you may not have
  thought of or tested for (“What happens if this list is empty? How do you
  know it’s not? Can these two threads deadlock? What if this server never
  responds?”).
- **Quality:** Programmers love criticizing other people’s code and picking
  little nits about why code is written one way or another. Although sometimes
  this can be annoying, it does yield code that has been looked through and
  criticized for any of its defects, with the important defects being fixed.
- **Style:** Having programmers review each others’ code ensures that all
  people in the company are using roughly the same style. In some languages
  this is not important, such as Go or Python, that have automatically enforced
  style checkers. However, in all cases, code reviews can make sure that style
  guidelines are actually being followed, once again leading to better
  maintainability.


## Actionable Advice:
From now on, categorize all of your repositories as *permanent* or *temporary*.
A temporary repository is one which you expect to be gone in the next month or
at most two, and thus don’t want to invest the effort in making sure it is high
quality and free of bugs. A permanent repository is one you plan on developing
and maintaining for at least several months into the future, and thus you want
to make sure it is high quality, well-documented, and easy to ramp new
programmers up in. You must never change a temporary repository to a permanent
repository, as that will result in low-quality unreviewed code becoming
permanent.


For all permanent repositories, all code that gets merged to the `master`
branch must go through a code review. In order to do a code review, push your
changes to a new branch on the repository. Then, make a pull request (PR) from
your branch to `master`, and tag at least one reviewer on the PR who will
review this PR. The PR can only be merged once at least one of the reviewer has
responded with a positive and complete review (often abbreviated “looks good to
me” or LGTM).

# Code Style
Although code style is a very surface-level characteristic of code, it is
nonetheless an important thing to think about, and it takes very little effort
to standardize style across the company and stick to a standard style.

## Reasons:
- **Cognitive Load:** When reading code, the majority of your effort should be on understanding what the code does; the data structures, the organization and architecture, the algorithms, the assumptions, etc. When multiple styles are in use, one gets distracted by minor things in the code, such as spacing, variable names, argument order, etc. Although these things are minor, they can cause a mental context switch, making it harder overall to parse and understand code.
- **Editing Code:** Coding style tends to be very subjective and by default changes from programmer to programmer. However, in your environment, people will regularly have to edit each other’s code. Without a company-wide enforced coding style, you will either end up with each project being a hodge-podge of different coding styles, or one will have to constantly adjust their coding style to match the project they are working on, depending on who initiated the project. This is another source of effort and mental load that is easily avoidable.

## Actionable Advice:
For each language in use, have an official and accepted coding style. Document
this style in a separate repository, and make sure that all new hires browse
these documents. During code reviews, remind people of coding style guidelines,
linking to specific clauses in the style guidelines as necessary. This will
gradually train people to conform to the standard coding styles, and although
it is an initial investment, it should gradually die down in required effort as
everyone gets used to the accepted styles.
	
Generally, coding guidelines should be adopted from existing language
guidelines, such as Python’s PEP8 or Google’s coding guidelines for Java. Most
languages have some documented style you can borrow and expand upon.

Finally, when at all possible, make sure that coding style is checked and
verified automatically. For all repositories, include a pre-commit hook that
checks files that were modified against the coding guidelines and flags any
commits that break coding style guidelines. Depending on how thorough you want
to be, you can have continuous integration tests run linters and style checkers
on the entire code base, thus making it very clear whether a PR meets your
style guidelines.
	
# Separation Between Development and Production
In order to avoid unexpected breakages, you should never edit running production code.

## Reasons:
- **Roadblocks for Others:** Your users (customers, other teams, yourselves)
  depend on the tools that you build for their own work. If you accidentally
  break the tools that they’re using, you can prevent them from being able to
  get things done. Unexpected breakages in production slow down not only you
  (because you have to drop everything and fix it), but also potentially the
  rest of the company, which can total to quite a few lost man-hours of effort.
- **Long-Term Codebase Quality:** Every time something breaks in production,
  you will rush to fix it as soon as possible, to avoid slowing down the rest
  of the company. In many cases, the fixes you apply will be stop-gap and badly
  thought-through. If you have an hour to fix something, you do not have time
  to stop and consider all the future implications of the fix, and potential
  alternative ways to do it. As a result, changing production regularly results
  in a less maintainable codebase in the long term, because you enter
  high-pressure hack-everything-into-a-working-state mode much more often than
  needed.

## Actionable Advice:
Any code that goes into production should be tested and verified on a separate
codebase first, and only then should it potentially go out to your users,
whoever they may be. The same applies to production databases and file storage
areas.

The way you implement this may vary throughout your codebases. In some cases,
you might have development machines that are automatically created clones of
production machines; in other cases, you can have continuous integration and
regression tests to verify you aren’t pushing untested code to production. You
will need to devise a separate strategy for each codebase you work with.

In addition, you want to make it very easy to do testing off of production, and
very hard to edit production thoughtlessly. This should be as effortless as
possible; scripts should exist for duplicating production databases and servers
and testing development branches on them, all PRs and commits should be
automatically tested against a clone of production, etc.

# Testing
All code should be rigorously tested before being committed to `master`. We
should aim for 100% code coverage, with all tests running automatically and
notifying the appropriate authors of any failures immediately.

## Reasons:
- **Code Quality:** Code written without the intention of testing it is often
  hard to test retroactively, because testing code requires that the code be
  modular: you must be able to replace certain parts of your code with fake
  ones, easily observe the behaviour of the code, etc. Thus, enforcing rigorous
  testing of your codebases forces us to architect your code better, taking
  into account modularity, observability, debuggability, and so on.
- **Catching Mistakes Early:** The main point of testing, of course, is to find
  mistakes early, before they become bugs in the codebase. Fixing a bug is
  often much more expensive than doing it right in the first place, as other
  parts of your code may now rely on the buggy behaviour or on the architecture
  that resulted in it. The earlier you catch mistakes, the easier it will be to
  fix them, and the less they will impact the company.
- **Development Speed:** A codebase that is well-tested can be iterated on
  much, much faster. If you are confident that tests will catch your mistakes,
  you can spend less mental cycles double-checking all of your work, and
  instead put effort into making sure that your code is well-architected,
  testable, etc. Although testing may seem expensive time-wise initially, the
  time-investment will pay off in the long run ten times over in frustration
  and developer time.
- **Flexibility:** Related to the previous point, a codebase that is
  well-tested is much more flexible. Tests can specify the behaviour of the
  code in a way that allows the implementation to change freely. Thus, if you
  have exhaustive tests for your code, you can easily refactor your codebase,
  knowing full well that mistakes will be caught by your tests. The end result
  is that your codebases are more adaptable to changing company conditions, and
  end up with a better architecture instead of layers and layers of hacks on
  top of an aging design.
- **New Hire Ramp-Up:** Once more related to the previous points, ramping up a
  new hire on a tested codebase can be much easier. New developers to a
  codebase are more likely to make mistakes when working on it, since they do
  not know all the ins and outs of the codebase and the edge cases that it must
  take into account. Tests can ensure that new developers cannot do too much
  damage when editing parts of the codebase they do not know, and also reduce
  the load on senior developers, as otherwise they would have to very
  thoroughly review any changes made by the newcomers.

## Actionable Advice:
As in the *Code Reviews* section, you designate repositories as temporary or
permanent. Permanent repositories will stay with you for a while, and so are
worth the investment to make them well-tested and designed.

There are many different types of tests, and you may want to use all of them in
different scenarios:

- **Unit Tests:** Unit tests are the most well-known type of testing. Unit tests are small snippets intended to test a very specific behaviors of a codebase. For example, a unit test may test that when a particular function is given certain inputs it returns certain outputs.
- **Regression Tests:** Regression tests are designed to avoid regressions in existing codebases, and are usually designed based on previous output or behaviour of a codebase. For example, a pipeline regression test may ensure that the output of certain parts of the pipeline are identical or within a small tolerance.
- **Performance Tests:** Performance tests are designed to make sure the performance of a codebase does not drop due to unintended consequences of changes you make. Performance tests can monitor time taken, CPU usage, number of cache hits, amount of memory used, number of minor and major garbage collections caused, etc. These should be used mostly when performance is actually important.
- **Property-Based Tests:** Property-based tests test that parts of the codebase obey certain properties in all conditions. They usually involve generating random (and sometimes targeted) inputs to functions or programs, and then verifying that the outputs of those programs obey certain properties. For example, if you generate random genomes and random reads from those genomes, you can test your aligner to verify that all the reads are properly localized; this is a property of the code that is independent of the inputs.
- **Integration Tests:** Integration tests test the behaviour and integrity of a system as a whole. These are meant to test the interaction between all the components in the system, verifying that all components communicated as expected, etc. Integration tests can involve building your projects and doing full run-throughs of your codebase, then verifying that the commands don’t throw any exceptions or exit with a non-zero exit code.
- **Fuzz Testing:** Fuzzing is a technique that involves handing programs randomly generated data in an attempt to give it unexpected or invalid input and verify that it handles it gracefully. This type of testing can be quite difficult to do well, so there are software programs that can assist with the generation of clever edge cases for any program.

For permanent codebases, you should develop a set of tests that will give us
assurances as to the quality of the codebase. Any new features must be
submitted as a PR, and each PR must include additions to the test suites that
test all of the new features as appropriate. Finally, you should sign up for
TravisCI (or a similar service) in order to do continuous integration testing.
This will allow you to easily verify that any PR or commit passes all the
relevant tests without having to go do the testing manually.

# Leveraging Version Control
You should leverage all the features of Git and Github (or whatever other
version control systems you use) to encourage good engineering. As you may have
guessed, we use Git with Github, but similar things apply to many other
systems.

## Reasons:
- **Git Hooks:** `git` allows us to specify hooks for all of its actions,
  inserting your own pre- and postprocessors. This can let us enforce data
  invariants, quality controls, and so on on your codebases.
- **Issue Discussions:** It’s great to be able to have a discussion related to
  the underlying issues associated with bugs or feature requests, issue
  progress updates, etc. In addition, the Github issue tracker allows us to
  upgrade your issue handling, allowing productive discussions (and remote
  discussions) on specific issues with your codebases, references to pull
  requests, and so on. Issue pages also provide a permanent repository of
  knowledge, both for current and closed issues; having this repository of
  permanent knowledge can be incredibly handy later on.
- **Code Reviews:** Using Github pull requests allows you to easily do code
  reviews from the web-based diff viewer, and lets people comment on specific
  issues and lines.

## Actionable Advice:
First, for your permanent repositories, you should create a set of git
hooks to ensure all possible quality checks have been passed. This can
include linting and style checking (with tooks such as pylint, pep8,
pyflakes, etc), spell checking comments and commit messages, making sure
temporary files aren’t checked in, making sure authentication tokens and
other passwords aren’t checked in, running quick tests, etc. On the
server-side, you can use tools such as TravisCI or CircleCI to run suites
long-running tests as soon as code has been pushed to a Github branch,
informing you of the results as soon as the tests are done. These tools
make it very easy to see when tests are failing or passing, and thus be
confident in merging a branch into `master` or requiring more debugging.

Second, you should begin heavily relying on Github PRs for branch management
and code reviews. There’s no particular reason to do this via PRs, but they are
a very convenient mechanism to have code reviews shared between multiple
participants, with a fairly easy viewing and commenting interface.

# Large-Scale Modularity
You should develop with an eye not only for small-scale codebase-local
modularity and code quality, but also for the way different codebases you
create interact and depend on each other, making sure that you pay attention to
large-scale modularity between the codebases.

## Reasons:
- **Modularity:** The benefits of small-scale modularity translate also to
  large-scale modularity. Modular codebases are easier to test, easier to
  develop on, easier to use, and more reliable in the long term.
- **Smooth Upgrade Paths:** By focusing on large-scale modularity, you avoid
  strong dependencies between your large codebases. As a result, a codebase can
  be easily swapped out for another, as long as the behaviour of both is
  roughly identical. This allows you to retire old codebases and technologies
  as your needs outgrow them, without having to also fix all your other
  codebases.
- **Technology Flexibility:** By ensuring that your components are modular and
  have well-defined interfaces, you free ourselves from the constraint that all
  components are written using the same languages, technologies, operating
  systems, etc. This allows you to use the right tool for the job.

## Actionable Advice:
Very carefully document the interfaces of all your codebases. Specifically,
each codebase should be well-defined in the ways one interacts with it, and
interactions should be strictly limited to those that are documented. Avoid
sharing code between codebases (except for general library code), as that
creates very tight coupling that can be very hard to undo later. Codebases
should communicate through well-defined and documented protocols, and nothing
else.

When possible, you should have these protocols defined through
language-agnostic REST APIs, with endpoints accepting JSON blobs describing
requests and serving JSON blobs as results. JSON blobs should be incredibly
well-defined, with every key present being documented, and allowing those keys
and only those keys. Communication protocols should be versioned, with the
version number being part of each JSON object, and with messages being rejected
if the versions of the client and the server do not match. (This same idea is
extensible to any other protocols and methods of communication – it does
not have to be HTTP or JSON based, and could just as easily be ZeroMQ,
inter-process communication, file-system based logging and messaging, etc.)
