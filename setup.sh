read -p "Project Name: " PROJECT_NAME
read -p "Project Description: " PROJECT_DESCRIPTION
read -p "Project Synopsis: " PROJECT_SYNOPSIS
read -p "Author Name: " AUTHOR_NAME
read -p "Author Github Username: " AUTHOR_USERNAME
read -p "Author email (for CI): " AUTHOR_EMAIL

find . -type f -exec sed -i "s/_PROJECT_SYNOPSIS_/$PROJECT_SYNOPSIS/g; s/_PROJECT_DESCRIPTION_/$PROJECT_DESCRIPTION/g; s/PROJECT_NAME_/$PROJECT_NAME/g; s/_AUTHOR_NAME_/$AUTHOR_NAME/g; s/_AUTHOR_USERNAME_/$AUTHOR_USERNAME/g; s/_AUTHOR_EMAIL_/$AUTHOR_EMAIL/g" {} +
echo "dir: ./docs" > "$PROJECT_NAME.odocl"
rm docs/PROJECT_NAME_.mld
echo -e "{0 Index}\n\nHello World!" > "docs/$PROJECT_NAME.mld"

rm setup.sh
rm PROJECT_NAME_.opam
