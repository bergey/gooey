set -e
for project in "$@"; do
    if ! git diff --quiet
    then
        echo "Please commit or stash changes before running $(basename $0)"
        exit 1
    fi
    rm -rf .gh-pages-build
    cd $project
    stack build
    cp -r $(stack path --local-install-root)/bin/${project}.jsexe ../.gh-pages-build
    cd ..
    mv ${project}/.stack-work .
    # store name of branch that we're leaving
    SRC_BRANCH=$(git rev-parse --abbrev-ref HEAD)
    SRC_COMMIT=$(git rev-parse HEAD)
    git checkout gh-pages
    rm -rf $project/
    mv .gh-pages-build $project
    git add $project
    git commit -m "build ${project} from ${SRC_COMMIT}"
    git checkout $SRC_BRANCH
    mv .stack-work $project
done
