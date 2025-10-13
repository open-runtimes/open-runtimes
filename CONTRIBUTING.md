
# Contributing

We would ❤️ for you to contribute to Open Runtimes and help make it better! We want contributing to Open Runtimes to be fun, enjoyable, and educational for anyone and everyone. All contributions are welcome, including issues, new docs as well as updates and tweaks, blog posts, workshops, and more.

## How to Start?

If you are worried or don’t know where to start, check out our next section explaining what kind of help we could use and where you can get involved. You can reach out with questions to [Eldad Fux (@eldadfux)](https://twitter.com/eldadfux) or anyone from the [Open Runtimes team on Discord](https://discord.gg/fP6W2qEzfQ). You can also submit an issue, and a maintainer can guide you!

## Code of Conduct

Help us keep Open Runtimes open and inclusive. Please read and follow our [Code of Conduct](/CODE_OF_CONDUCT.md).

## Submit a Pull Request 🚀

Branch naming convention is as following

`TYPE-ISSUE_ID-DESCRIPTION`

example:

```
doc-548-submit-a-pull-request-section-to-contribution-guide
```

When `TYPE` can be:

- **feat** - is a new feature
- **doc** - documentation only changes
- **cicd** - changes related to CI/CD system
- **fix** - a bug fix
- **refactor** - code change that neither fixes a bug nor adds a feature

**All PRs must include a commit message with the changes description!**

For the initial start, fork the project and use git clone command to download the repository to your computer. A standard procedure for working on an issue would be to:

1. `git pull`, before creating a new branch, pull the changes from upstream. Your master needs to be up to date.

```
$ git pull
```

2. Create new branch from `main` like: `doc-548-submit-a-pull-request-section-to-contribution-guide`<br/>

```
$ git checkout -b [name_of_your_new_branch]
```

3. Work - commit - repeat ( be sure to be in your branch )

4. Push changes to GitHub

```
$ git push origin [name_of_your_new_branch]
```

5. Submit your changes for review. If you go to your repository on GitHub, you'll see a `Compare & pull request` button. Click on that button.
6. Now submit the pull request and click on `Create pull request`.
7. Get a code review approval/reject
8. After approval, merge your PR
9. GitHub will automatically delete the branch after the merge is done. (they can still be restored).

## Installation

To install a working development environment follow these instructions:

1. Fork or clone the open-runtimes/open-runtimes repository.

2. Install Composer dependencies using one of the following options:

**Composer CLI**
```bash
composer update --ignore-platform-reqs --optimize-autoloader --no-plugins --no-scripts --prefer-dist
```

**Docker (UNIX)**

```bash
docker run --rm --interactive --tty --volume "$(pwd)":/app composer update --ignore-platform-reqs --optimize-autoloader --no-plugins --no-scripts --prefer-dist
```

**Docker (Windows)**

```bash
docker run --rm --interactive --tty --volume "%cd%":/app composer update --ignore-platform-reqs --optimize-autoloader --no-plugins --no-scripts --prefer-dist
```

4. Build the runtimes you want to test using the docker build command:
```
docker build -t {{Image Tag Name}} ./runtimes/{{Folder of your runtime}}
```

5. Follow our contribution guide to learn how you can add support for [more runtimes](docs/add-runtime.md) or [more versions](docs/add-version.md).

## Testing

We use PHP framework PHPUnit to test Open Runtimes. Every PR is automatically tested by Travis CI, and tests run for all runtimes.

Before running the tests, make sure to install all required PHP libraries:

```bash
docker run --rm --interactive --tty --volume $PWD:/app composer install
```

You also need to install [yq](https://github.com/mikefarah/yq):

```bash
brew install yq
```

> Alternatively run `go install github.com/mikefarah/yq/v4@latest` if brew isn't installed

To run test for a runtime, get runtime name (folder name) and execute below command:

```bash
bash tests.sh go-1.23
```

> Replace `go-1.23` with runtime name that you want to test

## Formatting

Run following command to run formatter for any runtime runtime:

```
sh formatter.sh node
```

> Replace `node` with runtime name that you want to test

## Introducing New Features

We would 💖 for you to contribute to Open Runtimes, but we would also like to make sure Open Runtimes is as great as possible and loyal to its vision and mission statement 🙏.

For us to find the right balance, please open an issue explaining your ideas before introducing a new pull request.

This will allow the Open Runtimes community to have sufficient discussion about the new feature value and how it fits in the product roadmap and vision.

This is also important for the Open Runtimes lead developers to be able to give technical input and different emphasis regarding the feature design and architecture.

## Other Ways to Help

Pull requests are great, but there are many other areas where you can help Open Runtimes.

### Blogging & Speaking

Blogging, speaking about, or creating tutorials about any of Open Runtimes’s many usages are great ways to contribute and help our project grow.

### Presenting at Meetups

Present at meetups and conferences about your Open Runtimes projects. Your unique challenges and successes in building things with Open Runtimes can provide great speaking material. We’d love to review your talk abstract/CFP, so get in touch with us if you’d like some help!

### Sending Feedbacks & Reporting Bugs

Sending feedback is a great way for us to understand your different use cases of Open Runtimes better. If you had any issues, bugs, or want to share about your experience, feel free to do so on our GitHub issues page or at our [Discord channel](https://discord.gg/fP6W2qEzfQ).

### Submitting New Ideas

If you think Open Runtimes could use a new feature, please open an issue on our GitHub repository, stating as much information as you can think about your new idea and its implications. We would also use this issue to gather more information, get more feedback from the community, and have a proper discussion about the new feature.

### Improving Documentation

Submitting documentation updates, enhancements, designs, or bug fixes. Spelling or grammar fixes will be very much appreciated.

### Helping Someone

Search for Open Runtimes on GitHub or StackOverflow and help someone else who needs help. You can also help by teaching others how to contribute to Open Runtimes's repo!
