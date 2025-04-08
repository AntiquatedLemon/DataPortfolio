#all requirements
import streamlit as st
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import random as rnd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, confusion_matrix
from sklearn.preprocessing import StandardScaler

#title
st.header("Interactive Data Web App")

#gimme data to ingest!!!
uploaded_file = st.file_uploader("Upload a CSV file", type=["csv"])

# If a file is uploaded, process it
if uploaded_file is not None:
    # Load data into a pandas dataframe
    df = pd.read_csv(uploaded_file)
    
    # Display the first few rows of the dataframe
    st.header("Dataset Preview:", divider="gray")
    st.dataframe(df.head(10))

    # Show basic information about the dataframe
    st.header("Basic Information:", divider = "gray")
    st.write(f"Shape: {df.shape[0]} by {df.shape[1]}")
    st.write(f"Column : Data Type")
    st.write(df.dtypes.astype(str).to_dict())

    st.write(f"Missing Values:")
    # Render only the columns that are missing values and how many
    nullseries = df.isnull().sum()
    st.write(nullseries[nullseries>0])
    
    # Show statistical summary
    st.write("Statistical Summary:")
    st.write(df.describe())
    
    # Visualize data using seaborn
    st.subheader("Data Visualization")
    
    # Plot a pairplot if there are less than 15 columns
    if df.shape[1] <= 15:
        sns.pairplot(df)
        st.pyplot() # future proof this later
    else:
        st.write("Pairplot skipped due to too many columns.")
        # Maybe let the user choose which pairplots they want to see?
  
  # Option to select columns for visualizations
    st.subheader("Choose Columns to Visualize")
    column_x = st.selectbox("Select Feature for X-axis", df.columns)
    column_y = st.selectbox("Select Feature for Y-axis", df.columns)
    
    if column_x and column_y:
        st.write(f"Scatter Plot of {column_x} vs {column_y}")
        fig, ax = plt.subplots()
        ax.scatter(df[column_x], df[column_y])
        ax.set_xlabel(column_x)
        ax.set_ylabel(column_y)
        st.pyplot(fig)

    # fix visualizations to be a bit more specific

    # machine learning access here

else:
    st.write("Please provide data!")

# please note that the AM flag does not show on chromium browsers
st.write("Thank you for exploring with me :flag-us::smile::hearts:")